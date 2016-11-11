package org.compevol.mcmc

object OneOnXPrior extends (Double => Double) {
  override def apply(x: Double): Double = - math.log(x)
}
