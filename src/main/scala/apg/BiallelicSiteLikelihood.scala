package apg

import apg.BiallelicSiteLikelihood.CachedF
import mcmc.Probability

import scala.collection.LinearSeq

class BiallelicSiteLikelihood(val piRed: Double, infiniteInterval: => InfiniteBiallelicCoalescentInterval, val partials: LinearSeq[Int => Double], val F: CachedF) extends Probability[Double] with Serializable {

  require(0 < piRed && piRed < 1.0)

  lazy val evaluate: Double = {
    val z = F.c
    println(math.log(z))
    z
  }

  def updatedCoalRate(i: Int, coalRate: Double, infiniteInterval: => InfiniteBiallelicCoalescentInterval): BiallelicSiteLikelihood = new BiallelicSiteLikelihood(piRed, infiniteInterval, partials, F.updatedCoalRate(i, coalRate))

}

object BiallelicSiteLikelihood {

  def apply(piRed: Double, infiniteInterval: => InfiniteBiallelicCoalescentInterval, intervals: LinearSeq[BiallelicCoalescentInterval], partials: LinearSeq[Int => Double]): BiallelicSiteLikelihood =
    new BiallelicSiteLikelihood(piRed, infiniteInterval, partials, createCachedF(intervals, partials))

  def createCachedF(intervals: LinearSeq[BiallelicCoalescentInterval], partials: LinearSeq[Int => Double]): CachedF = {
    val intervalsIterator = intervals.iterator
    val partialsIterator = partials.iterator
    var F: CachedF = new Base(intervalsIterator.next(), partialsIterator.next())
    while (intervalsIterator.hasNext) {
      val interval = intervalsIterator.next()
      F = new Nested(interval, if (interval.k > 0) partialsIterator.next() else _ => 0.0, F)
    }
    F
  }

  abstract class CachedF(fc: => (F, Double), val interval: BiallelicCoalescentInterval) extends Serializable {
    private[this] lazy val (fp, cp) = fc
    lazy val (f, c): (F, Double) = interval match {
      case interval: FiniteBiallelicCoalescentInterval =>
        val c = (for (n <- 1 to fp.N; r <- 0 to n) yield fp(n, r)).max
        val f = F(fp.N)
        for (n <- 1 to fp.N; r <- 0 to n) f(n, r) = fp(n, r) / c
        (Q.expQTtx(12, 1, interval.m, interval.u, interval.v, interval.coalRate, interval.length, f), c * cp)
      case interval: InfiniteBiallelicCoalescentInterval =>
        import spire.std.array._
        import spire.std.float._
        import spire.syntax.innerProductSpace._
        (null, cp * (fp.f dot interval.pi))
    }
    def updatedCoalRate(i: Int, coalRate: Double): CachedF
  }

  class Nested(interval: BiallelicCoalescentInterval, partial: Int => Double, previousF: CachedF) extends CachedF({
    val F = if (interval.k > 0) {
      val F = apg.F(interval.m)
      val k = interval.k
      for (i <- 0 to k; n <- 1 to previousF.f.N; r <- 0 to n)
        F(n + k, r + i) = F(n + k, r + i) + math.max(previousF.f(n, r), 0) * (partial(i) * HypergeometricPMF(n + k, r + i, k).apply(i)).toFloat
      F
    } else previousF.f
    (F, previousF.c)
  }, interval) with Serializable {

    def updatedCoalRate(i: Int, coalRate: Double): Nested = {
      val intervalp = if (interval.coalIndex == i) interval.updatedCoalRate(coalRate) else interval
      val previousFp = previousF.updatedCoalRate(i, coalRate)
      if ((interval ne intervalp) || (previousF ne previousFp))
        new Nested(intervalp, partial, previousFp)
      else
        this
    }

  }

  class Base(interval: BiallelicCoalescentInterval, baseF: => F) extends CachedF((baseF, 1), interval) with Serializable {

    def this(interval: BiallelicCoalescentInterval, partial: Int => Double) = this(interval, {
      val f = F(interval.m)
      for (r <- 0 to interval.m)
        f(interval.m, r) = partial(r).toFloat
      f
    })

    def updatedCoalRate(i: Int, coalRate: Double): Base = {
      if (interval.coalIndex == i)
        new Base(interval.updatedCoalRate(coalRate), baseF)
      else
        this
    }

  }

}
