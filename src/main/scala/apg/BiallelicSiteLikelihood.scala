package apg

import apg.BiallelicSiteLikelihood.CachedF
import mcmc.Probability
import org.apache.spark.broadcast.Broadcast
import snap.likelihood.MatrixExponentiator

import scala.collection.LinearSeq

class BiallelicSiteLikelihood(val piRed: Double, val infiniteInterval: Broadcast[InfiniteBiallelicCoalescentInterval], val partials: LinearSeq[Int => Double], val F: CachedF) extends Probability[Double] with Serializable {

  require(0 < piRed && piRed < 1.0)

  lazy val evaluate: Double = {
    import spire.std.array._
    import spire.std.double._
    import spire.syntax.innerProductSpace._
    val l = F.c * (F.f.asVectorCopyBase1() dot infiniteInterval.value.pi)
    val y = new F(1)
    y.set(1, 0, l)
    y.set(1, 1, l)
    F.f.get(1, 0) * (1 - piRed) + F.f.get(1, 1) * piRed
  }

  def updatedCoalRate(i: Int, coalRate: Double, infiniteInterval: Broadcast[InfiniteBiallelicCoalescentInterval]): BiallelicSiteLikelihood = new BiallelicSiteLikelihood(piRed, infiniteInterval, partials, F.updatedCoalRate(i, coalRate))

}

object BiallelicSiteLikelihood {

  def apply(piRed: Double, infiniteInterval: Broadcast[InfiniteBiallelicCoalescentInterval], intervals: LinearSeq[FiniteBiallelicCoalescentInterval], partials: LinearSeq[Int => Double]): BiallelicSiteLikelihood =
    new BiallelicSiteLikelihood(piRed, infiniteInterval, partials, createCachedF(intervals, partials))

  def createCachedF(intervals: LinearSeq[FiniteBiallelicCoalescentInterval], partials: LinearSeq[Int => Double]): CachedF = {
    val intervalsIterator = intervals.iterator
    val partialsIterator = partials.iterator
    var F: CachedF = new Base(intervalsIterator.next(), partialsIterator.next())
    while (intervalsIterator.hasNext) {
      val interval = intervalsIterator.next()
      F = new Nested(interval, if (interval.k > 0) partialsIterator.next() else _ => 0.0, F)
    }
    F
  }

  abstract class CachedF(fc: => (F, Double), val interval: FiniteBiallelicCoalescentInterval) extends Serializable {
    private[this] lazy val (fp, cp) = fc
    lazy val (f, c): (F, Double) = {
      val c = (for (n <- 1 to fp.getSize; r <- 0 to n) yield fp.get(n, r)).max
      val fpp = new F(fp.getSize)
      for (n <- 1 to fp.getSize; r <- 0 to n) fpp.set(n, r, fp.get(n, r) / c)
      (MatrixExponentiator.expQTtx(interval.m, interval.u, interval.v, interval.coalRate, interval.length, fpp), c * cp)
    }
    def updatedCoalRate(i: Int, coalRate: Double): CachedF
  }

  class Nested(interval: FiniteBiallelicCoalescentInterval, partial: Int => Double, previousF: CachedF) extends CachedF({
    val F = if (interval.k > 0) {
      val F = new F(interval.m)
      for (i <- 0 to interval.k; n <- 1 to previousF.f.getSize; r <- 0 to n)
        F.set(n + interval.k, r + i, F.get(n + interval.k, r + i) + math.max(previousF.f.get(n, r), 0) * partial(i) * HypergeometricPMF(n + interval.k, r + i, interval.k).apply(i))
      F
    } else previousF.f
    (F, previousF.c)
  }, interval) with Serializable {

    def updatedCoalRate(i: Int, coalRate: Double): Nested = {
      val intervalp = if (interval.coalIndex == i) interval.updatedCoalRate(coalRate) else interval
      val previousFp = previousF.updatedCoalRate(i, coalRate)
      if ((interval ne interval) || (previousF ne previousFp))
        new Nested(intervalp, partial, previousFp)
      else
        this
    }

  }

  class Base(interval: FiniteBiallelicCoalescentInterval, f: => F) extends CachedF((f, 1), interval) with Serializable {

    def this(interval: FiniteBiallelicCoalescentInterval, partial: Int => Double) = this(interval, {
      val F = new F(interval.m)
      for (r <- 0 to interval.m)
        F.set(interval.m, r, partial(r))
      F
    })

    def updatedCoalRate(i: Int, coalRate: Double): Base = {
      if (interval.coalIndex == i)
        new Base(interval.updatedCoalRate(coalRate), f)
      else
        this
    }

  }

}
