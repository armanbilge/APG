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
    F.c * (F.f.asVectorCopyBase1() dot infiniteInterval.value.pi)
  }

  def updatedCoalRate(i: Int, coalRate: Double, infiniteInterval: Broadcast[InfiniteBiallelicCoalescentInterval]): BiallelicSiteLikelihood = new BiallelicSiteLikelihood(piRed, infiniteInterval, partials, F.updatedCoalRate(i, coalRate))

}

object BiallelicSiteLikelihood {

  def apply(piRed: Double, infiniteInterval: Broadcast[InfiniteBiallelicCoalescentInterval], intervals: LinearSeq[BiallelicCoalescentInterval], partials: LinearSeq[Int => Double]): BiallelicSiteLikelihood =
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
        val c = (for (n <- 1 to fp.getSize; r <- 0 to n) yield fp.get(n, r)).max
        val f = new F(fp.getSize)
        for (n <- 1 to fp.getSize; r <- 0 to n) f.set(n, r, fp.get(n, r) / c)
        (MatrixExponentiator.expQTtx(interval.m, interval.u, interval.v, interval.coalRate, interval.length, f), c * cp)
      case interval: InfiniteBiallelicCoalescentInterval => (fp, cp)
    }
    def updatedCoalRate(i: Int, coalRate: Double): CachedF
  }

  class Nested(interval: BiallelicCoalescentInterval, partial: Int => Double, previousF: CachedF) extends CachedF({
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

  class Base(interval: BiallelicCoalescentInterval, baseF: => F) extends CachedF((baseF, 1), interval) with Serializable {

    def this(interval: BiallelicCoalescentInterval, partial: Int => Double) = this(interval, {
      val F = new F(interval.m)
      for (r <- 0 to interval.m)
        F.set(interval.m, r, partial(r))
      F
    })

    def updatedCoalRate(i: Int, coalRate: Double): Base = {
      if (interval.coalIndex == i)
        new Base(interval.updatedCoalRate(coalRate), baseF)
      else
        this
    }

  }

}
