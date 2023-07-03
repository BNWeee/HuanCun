package huancun.prefetch

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink._
import huancun._
import huancun.utils.Pipeline

abstract class PrefetchRecv extends Bundle {
  val addr = UInt(64.W)
  val addr_valid = Bool()
  val pf_en = Bool()
}

class l2PrefetchRecv extends PrefetchRecv {
  val l2_pf_en = Bool()
}

class l3PrefetchRecv extends PrefetchRecv{
  val l3_pf_en = Bool()
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
  io.recv_addr:=DontCare
  io.req.bits.tag := parseFullAddress(io.recv_addr.bits)._1
  io.req.bits.set := parseFullAddress(io.recv_addr.bits)._2
  io.req.bits.needT := false.B
  io.req.bits.isBOP := false.B
  io.req.bits.source := 5.U //FIXME: ensure source id is l2cache
  io.req.valid := io.recv_addr.valid
  io.req.ready := DontCare
}