package Sodor

import Chisel._
import Node._
import Constants._
import Common._
import Common.Util._
import ReferenceChipBackend._
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap


object ReferenceChipBackend {
  val initMap = new HashMap[Module, Bool]()
}

class TopIO() extends Bundle  {
  val debug_stats_csr = Bool(OUTPUT)
  val htif  = new Common.HTIFIO()
}

class Top extends Module 
{
   val io = new TopIO()

   implicit val sodor_conf = SodorConfiguration()

   val reset_signal = Reg(next=Reg(next=io.htif.reset))
   val tile = Module(new SodorTile)
  
   tile.io.host.reset := reset_signal
   tile.io.host.id := UInt(0,1)
   tile.io.host.csr_req <> Queue(io.htif.csr_req)
   io.htif.csr_rep <> Queue(tile.io.host.csr_rep)

   tile.io.host.mem_req <> Queue(io.htif.mem_req)
   io.htif.mem_rep <> tile.io.host.mem_rep

   io.debug_stats_csr := Reg(next=tile.io.host.debug_stats_csr)
}

class CoreWrapper(implicit conf: SodorConfiguration) extends Module
{
  val io = new Bundle {
    val reset = Bool(INPUT)
    val host = new HTIFIO()
    val imem = new MemPortIo(conf.xprlen)
    val dmem = new MemPortIo(conf.xprlen)
  }
  
  val core = Module(new Core(io.reset))
  
  core.io.host <> io.host
  core.io.imem <> io.imem
  core.io.dmem <> io.dmem
}


class CoreTests(c: CoreWrapper) extends Tester(c) {
  import c.io._ 
   
  def checkCoreOutputs(imem_req_addr: Option[Int] = Some(0x2000),
                       dmem_req_addr: Option[Int] = None,
                       host_ipi_req_valid: Int = 0, 
                       host_csr_rep_valid: Int = 0) {

    imem_req_addr match {
      case Some(addr) =>
        expect(imem.req.valid, 1)
        expect(imem.req.bits.addr, addr)
      case None =>
        expect(imem.req.valid, 0)
    }

    dmem_req_addr match {
      case Some(addr) =>
        expect(dmem.req.valid, 1)
        expect(dmem.req.bits.addr, addr)
      case None =>
        expect(dmem.req.valid, 0)    
    }

    expect(host.ipi_req.valid, host_ipi_req_valid)
    expect(host.csr_rep.valid, host_csr_rep_valid)
  }
  
  poke(c.io.reset, 1)
  poke(imem.req.ready, 0)
  poke(dmem.req.ready, 0)
  poke(host.csr_req.valid, 0)
  poke(host.ipi_rep.valid, 0)
  
  //Check stable reset behaviour
  for (i <- 1 to 10) {
    step(1)
    checkCoreOutputs()
  }
  
  poke(c.io.reset, 0)
  poke(imem.req.ready, 1)
  poke(imem.resp.valid, 1)
  poke(dmem.req.ready, 0)
  poke(dmem.resp.valid, 0)
  poke(imem.resp.bits.data, 0x6f) //Branch to self
  
  for (i <- 1 to 5) {
    step(1)
    checkCoreOutputs(imem_req_addr = Some(0x2004))
    step(1)
    checkCoreOutputs(imem_req_addr = Some(0x2000))
  }
  
  //                         ADDR             W             rd          LD
  poke(imem.resp.bits.data, (0x123 << 20) | (0x2 << 12) | (0x1 << 7) | 0x03)
  step(1)
  checkCoreOutputs(imem_req_addr = Some(0x2004), dmem_req_addr = Some(0x123))
  
  
  
  
  val (addrhi, addrlo) = ((0x321 >> 5), 0x321 & 0x1f)
  //                         ADDRHI           rs             W            ADDRLO          ST
  //poke(imem.resp.bits.data, (addrhi << 25) | (0x1 << 20) | (0x2 << 12) | (addrlo << 7) | 0x23)
  step(1)
  checkCoreOutputs(imem_req_addr = Some(0x2008),    //No stall despite no dmem valid.
                   dmem_req_addr = Some(0x123))
 
 
 
 
 /*
  poke(imem.resp.bits.data, 0x6f) //Branch to self  
  step(1) 
  checkCoreOutputs(imem_req_addr = Some(0x200c), dmem_req_addr = Some(0x321))
 
 
  step(1)
  checkCoreOutputs(imem_req_addr = Some(0x2008))
 
 
  step(1)
  checkCoreOutputs(imem_req_addr = Some(0x200c))*/
  
}


object elaborate {
  def main(args: Array[String]): Unit = {
    //chiselMain(args, () => Module(new Top()))
    implicit val sodor_conf = SodorConfiguration()

     chiselMainTest(args, () => Module(new CoreWrapper())) {
      c => new CoreTests(c) }
  }
}
