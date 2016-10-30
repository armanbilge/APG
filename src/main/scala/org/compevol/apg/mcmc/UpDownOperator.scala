package org.compevol.apg.mcmc

import org.apache.commons.math3.random.RandomGenerator

class UpDownOperator(val scaleFactor: Double)(implicit random: RandomGenerator) extends Operator[(Seq[Double], Seq[Double])] {

  override def apply(t: (Seq[Double], Seq[Double])): (Seq[Double], Seq[Double]) = {
    val scale = scaleFactor + random.nextDouble() * (1 / scaleFactor - scaleFactor)
    (t._1.map(_ * scale), t._1.map(_ / scale))
  }

  override def logHastingsRatio(x: (Seq[Double], Seq[Double]), y: (Seq[Double], Seq[Double])): Double = (x._1.length - x._2.length - 2) * math.log(y._1.head / x._1.head)

}
