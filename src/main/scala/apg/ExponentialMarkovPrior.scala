package apg

import mcmc.Probability
import monocle.Lens
import monocle.function.At
import org.apache.commons.math3.distribution.{GammaDistribution, LogNormalDistribution}

class ExponentialMarkovPrior[T <: Double, S <: Double, C <: Traversable[T]](val chain: C, val shape: S) extends Probability[Double] {

  lazy val evaluate: Double = {
    new LogNormalDistribution(null, 12.7, 1.75).logDensity(chain.head) + chain.toIterator.sliding(2).withPartial(false).map { t =>
      val mean = t.head
      val x = t.tail.head
      val scale = mean / shape
      new GammaDistribution(null, shape, scale).logDensity(x)
    }.sum
  }

}

object ExponentialMarkovPrior {

  def apply[T <: Double, S <: Double, C <: Traversable[T]](chain: C, shape: S) = new ExponentialMarkovPrior[T, S, C](chain, shape)

  implicit def shape[T <: Double, S <: Double, C <: Traversable[T]]: Lens[ExponentialMarkovPrior[T, S, C], S] = Lens[ExponentialMarkovPrior[T, S, C], S](_.shape)(s => emp => new ExponentialMarkovPrior[T, S, C](emp.chain, s))

  implicit def chain[T <: Double, S <: Double, C <: Traversable[T]]: Lens[ExponentialMarkovPrior[T, S, C], C] = Lens[ExponentialMarkovPrior[T, S, C], C](_.chain)(c => emp => new ExponentialMarkovPrior[T, S, C](c, emp.shape))

  implicit def atChain[T <: Double, S <: Double, C <: Traversable[T], I](implicit atC: At[C, I, T]): At[ExponentialMarkovPrior[T, S, C], I, T] = new At[ExponentialMarkovPrior[T, S, C], I, T] {
    override def at(i: I): Lens[ExponentialMarkovPrior[T, S, C], T] = chain ^|-> atC.at(i)
  }

}
