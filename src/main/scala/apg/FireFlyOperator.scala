package apg

import mcmc.{Operator, Probability}
import spire.random.Generator
import spire.random.rng.MersenneTwister64

import scala.language.higherKinds

class FireFlyOperator[D[X], Z[X], B, P <: Probability[Double], L <: Probability[Double]](val `q_d->b`: Double, val rng: Array[Long] => Generator = MersenneTwister64.fromArray)(implicit distributed: Distributed[D, Z]) extends Operator[D[DatumLikelihood[B, P, L]], Double] {

  override def apply(d: D[DatumLikelihood[B, P, L]]): D[DatumLikelihood[B, P, L]] = {
    val `q_d->b` = this.`q_d->b`
    val rng = this.rng
    import distributed._
    d.synchronizedMap(i => rng(Array(System.nanoTime() + i * (1 << 14)))) { rng =>
      { dl =>
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
          if (Ltilde / `q_d->b` > u)
            dl.flipped
          else
            dl
        } else
          dl
      }
    }.persist()
  }

  override def hastingsRatio(x: D[DatumLikelihood[B, P, L]], y: D[DatumLikelihood[B, P, L]]): Double = {
    import distributed._
    x.zipMap(y)(_.evaluate - _.evaluate).sum
  }

}
