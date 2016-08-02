package org.compevol.apg

case class TimePoint(t: Double, redCountPartial: IndexedSeq[Double]) extends Ordered[TimePoint] {
  override def compare(that: TimePoint): Int = this.t compare that.t
  val k = redCountPartial.size - 1
}
