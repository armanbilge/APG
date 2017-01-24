package apg

import mcmc.{Operator, Probability}
import spire.random.Generator

import scala.collection.generic.CanBuildFrom

class FireFlyOperator[B, P <: Probability[Double], L <: Probability[Double], C <: Iterable[DatumLikelihood[B, P, L]]](val `q_d->b`: Double)(implicit rng: Generator, cbf: CanBuildFrom[Iterable[DatumLikelihood[B, P, L]], DatumLikelihood[B, P, L], C]) extends Operator[C, Double] {

  override def apply(c: C): C = c.map[DatumLikelihood[B, P, L], C] { dl =>
    if (dl.lit) {
      val u = rng.nextDouble()
      val Ltilde = (dl.probability.evaluate - dl.lower.evaluate) / dl.lower.evaluate
      if (`q_d->b` / Ltilde > u)
        dl.flipped
      else
        dl
    } else if (rng.nextDouble() < `q_d->b`) {
      val u = rng.nextDouble()
      val Ltilde = (dl.probability.evaluate - dl.lower.evaluate) / dl.lower.evaluate
      if (Ltilde / `q_d->b` < u)
        dl.flipped
      else
        dl
    } else
      dl
  }

  override def hastingsRatio(x: C, y: C): Double = (x, y).zipped.map { (x: DatumLikelihood[B, P, L], y: DatumLikelihood[B, P, L]) =>
    x.evaluate - y.evaluate
  }.sum

}
