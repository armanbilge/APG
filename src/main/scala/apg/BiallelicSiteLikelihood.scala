package apg

import apg.BiallelicSiteLikelihood.CachedF
import mcmc.Probability

import scala.collection.LinearSeq

class BiallelicSiteLikelihood(val piRed: Double, infiniteInterval: => InfiniteBiallelicCoalescentInterval, val partials: LinearSeq[Int => Double], val F: CachedF) extends Probability[Double] with Serializable {

  require(0 < piRed && piRed < 1.0)

  lazy val evaluate: Double = F.getFC._2

  def updatedCoalRate(i: Int, coalRate: Double, infiniteInterval: => InfiniteBiallelicCoalescentInterval): BiallelicSiteLikelihood = new BiallelicSiteLikelihood(piRed, infiniteInterval, partials, F.updatedCoalRate(i, coalRate))

}

object BiallelicSiteLikelihood {

  def apply(piRed: Double, infiniteInterval: => InfiniteBiallelicCoalescentInterval, intervals: LinearSeq[BiallelicCoalescentInterval], partials: LinearSeq[Int => Double]): BiallelicSiteLikelihood =
    new BiallelicSiteLikelihood(piRed, infiniteInterval, partials, createCachedF(intervals, partials))

  def createCachedF(intervals: LinearSeq[BiallelicCoalescentInterval], partials: LinearSeq[Int => Double]): CachedF = {
    val intervalsIterator = intervals.iterator
    val nextIntervalsIterator = intervals.iterator.drop(1)
    val partialsIterator = partials.iterator
    def handleIntervals() = {
      val interval = intervalsIterator.next()
      val nextInterval = if (nextIntervalsIterator.hasNext) Some(nextIntervalsIterator.next()) else None
      (interval, nextInterval.exists(_.coalIndex != interval.coalIndex))
    }
    val (int, cache) = handleIntervals()
    var F: CachedF = new Base(int, partialsIterator.next(), cache)
    while (intervalsIterator.hasNext) {
      val (int, cache) = handleIntervals()
      F = new Nested(int, if (int.k > 0) partialsIterator.next() else _ => 0.0, F, cache)
    }
    F
  }

  abstract class CachedF(fc: => (F, Double), val interval: BiallelicCoalescentInterval, cache: Boolean) extends Serializable {
    private[this] var cachedFC: (F, Double) = null
    def getFC: (F, Double) = if (cachedFC != null) {
      cachedFC
    } else {
      val calculatedFC = {
        val (fp, cp) = fc
        interval match {
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
      }
      if (cache) {
        cachedFC = calculatedFC
      }
      calculatedFC
    }
    def updatedCoalRate(i: Int, coalRate: Double): CachedF
  }

  class Nested(interval: BiallelicCoalescentInterval, partial: Int => Double, previousF: CachedF, cache: Boolean) extends CachedF({
    val (previousf, previousc) = previousF.getFC
    val F = if (interval.k > 0) {
      val F = apg.F(interval.m)
      val k = interval.k
      for (i <- 0 to k; n <- 1 to previousf.N; r <- 0 to n)
        F(n + k, r + i) = F(n + k, r + i) + math.max(previousf(n, r), 0) * (partial(i) * HypergeometricPMF(n + k, r + i, k).apply(i)).toFloat
      F
    } else previousf
    (F, previousc)
  }, interval, cache) with Serializable {

    def updatedCoalRate(i: Int, coalRate: Double): Nested = {
      val intervalp = if (interval.coalIndex == i) interval.updatedCoalRate(coalRate) else interval
      val previousFp = previousF.updatedCoalRate(i, coalRate)
      if ((interval ne intervalp) || (previousF ne previousFp))
        new Nested(intervalp, partial, previousFp, cache)
      else
        this
    }

  }

  class Base(interval: BiallelicCoalescentInterval, baseF: F, cache: Boolean) extends CachedF((baseF, 1), interval, cache) with Serializable {

    def this(interval: BiallelicCoalescentInterval, partial: Int => Double, cache: Boolean) = this(interval, {
      val f = F(interval.m)
      for (r <- 0 to interval.m)
        f(interval.m, r) = partial(r).toFloat
      f
    }, cache)

    def updatedCoalRate(i: Int, coalRate: Double): Base = {
      if (interval.coalIndex == i)
        new Base(interval.updatedCoalRate(coalRate), baseF, cache)
      else
        this
    }

  }

}
