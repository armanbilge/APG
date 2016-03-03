package org.compevol.apg

case class BiallelicSample(t: Double, redProbs: Map[Taxon, Double]) extends Ordered[BiallelicSample] {
  override def compare(that: BiallelicSample): Int = this.t compare that.t
}
