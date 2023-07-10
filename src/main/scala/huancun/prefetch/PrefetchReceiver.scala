package huancun.prefetch

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.{BundleBridgeSink,BundleBridgeSource, LazyModule, LazyModuleImp}
import freechips.rocketchip.tilelink._
import huancun._
import huancun.utils.{Pipeline, RegNextN}

abstract class PrefetchRecv extends Bundle {
  val addr = UInt(64.W)
  val addr_valid = Bool()
  val pf_en = Bool()
}

class l2PrefetchRecv extends PrefetchRecv {
}
class l2PrefetchSend extends PrefetchRecv {
}
class l3PrefetchRecv extends Bundle{
  val data=Vec(2,new l2PrefetchRecv())
}

case class PrefetchReceiverParams(n: Int = 32) extends PrefetchParameters {
  override val hasPrefetchBit: Boolean = true
  override val inflightEntries: Int = n
}

class PrefetchReceiver()(implicit p: Parameters) extends PrefetchModule {
  val io = IO(new PrefetchIO())
  // just ignore train reqs
  io.train.ready := true.B
  io.resp.ready := true.B

  io.req.bits.tag := parseFullAddress(io.recv_addr.bits)._1
  io.req.bits.set := parseFullAddress(io.recv_addr.bits)._2
  io.req.bits.needT := false.B
  io.req.bits.isBOP := false.B
  io.req.bits.source := 0.U // TODO: ensure source 0 is dcache
  io.req.valid := io.recv_addr.valid

}

class PrefetchReceiver_llc()(implicit p: Parameters) extends PrefetchModule {
  val io = IO(new PrefetchIO())
  io.train:=DontCare
  io.resp:=DontCare

  io.req.valid :=RegNextN(io.recv_addr.valid,3)
  io.req.bits.tag := RegNextN(parseAddress(io.recv_addr.bits)._1,3)
  io.req.bits.set := RegNextN(parseAddress(io.recv_addr.bits)._2,3)
  io.req.bits.needT := RegNextN(false.B,3)
  io.req.bits.isBOP := RegNextN(false.B,3)
  io.req.bits.source := RegNextN(72.U,3) //FIXME: ensure source id is l2cache
  io.req.ready := DontCare
}

class PrefetchReceiverXbar(val clientNum:Int=2)(implicit p: Parameters) extends LazyModule{
  val inNode = Seq.fill(clientNum)(BundleBridgeSink(Some(() => new huancun.prefetch.l2PrefetchRecv())))
  val outNode = Seq.fill(1)(BundleBridgeSource(Some(() => new huancun.prefetch.l2PrefetchRecv())))
  lazy val module = new LazyModuleImp(this){
    val arbiter = Module(new Arbiter(new huancun.prefetch.l2PrefetchRecv(), clientNum))
    arbiter.suggestName(s"pf_l3recv_node_arb")
    for (i <- 0 until clientNum) {
      arbiter.io.in(i).valid := inNode(i).in.head._1.addr_valid
      arbiter.io.in(i).bits.addr_valid := inNode(i).in.head._1.addr_valid
      arbiter.io.in(i).bits.addr := inNode(i).in.head._1.addr
      arbiter.io.in(i).bits.pf_en := inNode(i).in.head._1.pf_en
      arbiter.io.in(i).ready := DontCare
    }
    outNode.head.out.head._1 <> arbiter.io.out.bits
    arbiter.io.out.ready := true.B
  }
}