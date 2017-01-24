package apg

import mcmc.Probability
import monocle.Lens

class OneOnXPrior[T <: Double](val x: T) extends Probability[Double] {
  override val evaluate: Double = - math.log(x)
}

object OneOnXPrior {

  def apply[T <: Double](x: T): OneOnXPrior[T] = new OneOnXPrior(x)

  def x[T <: Double]: Lens[OneOnXPrior[T], T] =
    Lens[OneOnXPrior[T], T](_.x)(x => _ => OneOnXPrior(x))

}
