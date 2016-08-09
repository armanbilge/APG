package org.compevol.apg

import java.io.File

import scala.io.Source

case class TimePoint(t: Double, redCountPartial: IndexedSeq[Double]) extends Ordered[TimePoint] {
  override def compare(that: TimePoint): Int = this.t compare that.t
  val k = redCountPartial.size - 1
}

object TimePoint {

  def fromFile(t: Double, f: File): Array[TimePoint] =
    Source.fromFile(f).getLines.map(l => TimePoint(t, l.split("\\s").map(_.toDouble).toIndexedSeq)).toArray

}
