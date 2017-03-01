package apg

import mcmc.{Operator, Probability}
import org.apache.spark.rdd.RDD
import spire.random.Generator
import spire.random.rng.MersenneTwister64

class FireFlyOperator[B, P <: Probability[Double], L <: Probability[Double]](val `q_d->b`: Double) extends Operator[RDD[DatumLikelihood[B, P, L]], Double] {

  override def apply(c: RDD[DatumLikelihood[B, P, L]]): RDD[DatumLikelihood[B, P, L]] = {
    val `q_d->b` = this.`q_d->b`
    c.mapPartitionsWithIndex { (i, dls) =>
      val rng = MersenneTwister64.fromTime()
      dls.map { dl =>
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

  override def hastingsRatio(x: RDD[DatumLikelihood[B, P, L]], y: RDD[DatumLikelihood[B, P, L]]): Double = x.zip(y).map(Function.tupled { (x: DatumLikelihood[B, P, L], y: DatumLikelihood[B, P, L]) =>
    x.evaluate - y.evaluate
  }).sum

}
