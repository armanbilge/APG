package apg

import mcmc.{JointProbability, Probability}
import monocle.Lens
import shapeless.tag
import shapeless.tag.@@
import spire.std.double._

class DatumLikelihood[B, P <: Probability[Double], L <: Probability[Double]](val lit: Boolean @@ B, val probability: P, val lower: L) extends JointProbability[Double, P, L](probability, lower) {

  override lazy val evaluate: Double = math.log(if (lit) math.max(probability.evaluate - lower.evaluate, 0) else lower.evaluate)

  lazy val flipped: DatumLikelihood[B, P, L] = new DatumLikelihood(tag[B](!lit), probability, lower)

}

object DatumLikelihood {

  implicit def lit[B, P <: Probability[Double], L <: Probability[Double]]: Lens[DatumLikelihood[B, P, L], Boolean @@ B] =
    Lens[DatumLikelihood[B, P, L], Boolean @@ B](_.lit)(b => dl => new DatumLikelihood(b, dl.probability, dl.lower))

}
