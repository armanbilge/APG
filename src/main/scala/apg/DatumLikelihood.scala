package apg

import mcmc.Probability
import monocle.Lens
import shapeless.tag
import shapeless.tag.@@

class DatumLikelihood[B, P <: Probability[Double], L1 <: Probability[Double], L2 <: Probability[Double]](val lit: Int @@ B, val probability: P, val lower1: L1, val lower2: L2) extends Probability[Double] {

  override lazy val evaluate: Double = math.log(lit.toInt match {
    case 0 => lower2.evaluate
    case 1 => math.max(lower1.evaluate - lower2.evaluate, 0)
    case 2 => math.max(probability.evaluate - lower1.evaluate, 0)
  })

  def light(b: Int @@ B): DatumLikelihood[B, P, L1, L2] = if (lit == b) this else new DatumLikelihood(b, probability, lower1, lower2)
  lazy val on: DatumLikelihood[B, P, L1, L2] = light(tag[B](2))
  lazy val dim: DatumLikelihood[B, P, L1, L2] = light(tag[B](1))
  lazy val off: DatumLikelihood[B, P, L1, L2] = light(tag[B](0))

  lazy val entropy: Double = - on.evaluate * math.log(on.evaluate) - dim.evaluate * math.log(dim.evaluate) - off.evaluate - math.log(dim.evaluate)

}

object DatumLikelihood {

  implicit def lit[B, P <: Probability[Double], L1 <: Probability[Double], L2 <: Probability[Double]]: Lens[DatumLikelihood[B, P, L1, L2], Int @@ B] =
    Lens[DatumLikelihood[B, P, L1, L2], Int @@ B](_.lit)(b => dl => new DatumLikelihood(b, dl.probability, dl.lower1, dl.lower2))

}
