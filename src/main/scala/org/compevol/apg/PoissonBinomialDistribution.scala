package org.compevol.apg

class PoissonBinomialDistribution(val p: Seq[Double]) {

  val w = p.map(x => x / (1 - x)).toVector



}
