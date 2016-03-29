package org.compevol.apg.mcmc

object OneOnXPrior extends (Double => Double) {
  override def apply(x: Double): Double = - math.log(x)
}
