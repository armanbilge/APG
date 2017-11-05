package apg

import scala.collection.LinearSeq
import scala.language.higherKinds

object Sites {

  def apply[D[X], B[X]](timePoints: Seq[Array[TimePoint]], threshold: Double = 0)(implicit distributed: Distributed[D, B]): D[LinearSeq[TimePoint]] = {
    import distributed._
    val broadcastedTimePoints = distributed.broadcast(timePoints)
    distributed.range(0, timePoints.head.length).map(_.toInt).map(i => distributed.retrieve(broadcastedTimePoints).map(_(i)).sorted.to[LinearSeq]).filter(_.forall(_.redCountPartial.exists(_ > threshold)))
  }

}
