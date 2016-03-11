package org.compevol.apg.mcmc

object OnOnXPrior extends (Double => Double) {
  override def apply(x: Double): Double = - math.log(x)
}
