package org.compevol.apg.mcmc

import scala.util.Random

class ScaleOperator(val scaleFactor: Double)(implicit _random: Random) extends Operator[Seq[Double]] {

  override def apply(t: Seq[Double]): Seq[Double] = {
    val scale = scaleFactor + random.nextDouble() * (1 / scaleFactor - scaleFactor)
    t.map(_ * scale)
  }

  override def logHastingsRatio(x: Seq[Double], y: Seq[Double]): Double = (x.length - 2) * math.log(y.head / x.head)

}
