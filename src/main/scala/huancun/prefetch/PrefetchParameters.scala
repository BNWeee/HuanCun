package huancun.prefetch

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import huancun._

trait PrefetchParameters {
  val hasPrefetchBit:  Boolean
  val inflightEntries: Int // max num of inflight prefetch reqs
}

trait HasPrefetchParameters extends HasHuanCunParameters {
  val inflightEntries = if(prefetchOpt.nonEmpty)prefetchOpt.get.inflightEntries
  else if(prefetchRecvOpt.nonEmpty)prefetchRecvOpt.get.inflightEntries
  else 16
}

abstract class PrefetchBundle(implicit val p: Parameters) extends Bundle with HasPrefetchParameters
abstract class PrefetchModule(implicit val p: Parameters) extends Module with HasPrefetchParameters
