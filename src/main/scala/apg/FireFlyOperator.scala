package apg

import mcmc.{Categorical, Operator, Probability}
import spire.random.Generator

import scala.language.higherKinds

class FireFlyOperator[D[X], Z[X], B, P <: Probability[Double], L1 <: Probability[Double], L2 <: Probability[Double]](val tweakProb: Long => Double)(implicit distributed: Distributed[D, Z], rng: Generator) extends Operator[D[DatumLikelihood[B, P, L1, L2]], Double] {

  override def apply(d: D[DatumLikelihood[B, P, L1, L2]]): D[DatumLikelihood[B, P, L1, L2]] = {
    val tweakProb = this.tweakProb
    val rng = this.rng
    import distributed._
    d.zipWithIndexMap { (dl, i) =>
      if (rng.nextDouble() < tweakProb(i)) {
        val on = dl.on
        val dim = dl.dim
        val off = dl.off
        import spire.std.double._
        rng.next(Categorical(Map(on -> math.exp(on.evaluate), dim -> math.exp(dim.evaluate), off -> math.exp(off.evaluate))))
      } else
        dl
    }.persist()
  }

  override def hastingsRatio(x: D[DatumLikelihood[B, P, L1, L2]], y: D[DatumLikelihood[B, P, L1, L2]]): Double = {
    import distributed._
    x.map(_.evaluate).sum - y.map(_.evaluate).sum
  }

}
