package apg

import apg.BiallelicSiteLikelihood.CachedF
import mcmc.Probability
import snap.likelihood.MatrixExponentiator

import scala.collection.LinearSeq

class BiallelicSiteLikelihood(val piRed: Double, val F: CachedF) extends Probability[Double] {

  require(0 < piRed && piRed < 1.0)

  lazy val evaluate = F.f.get(1, 0) * (1 - piRed) + F.f.get(1, 1) * piRed

  def updatedCoalRate(i: Int, coalRate: Double): BiallelicSiteLikelihood = new BiallelicSiteLikelihood(piRed, F.updatedCoalRate(i, coalRate))

}

object BiallelicSiteLikelihood {

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

  abstract class CachedF(val fc: (F, Double), val interval: BiallelicCoalescentInterval) {
    private[this] val (fp, cp) = fc
    lazy val (f, c): (F, Double) = interval match {
      case infiniteInterval: InfiniteBiallelicCoalescentInterval =>
        import spire.std.array._
        import spire.std.double._
        import spire.syntax.innerProductSpace._
        val l = cp * (fp.asVectorCopyBase1() dot infiniteInterval.pi)
        val y = new F(1)
        y.set(1, 0, l)
        y.set(1, 1, l)
        (y, 1)
      case finiteInterval: FiniteBiallelicCoalescentInterval =>
        val cp = (for (n <- 1 to fp.getSize; r <- 0 to n) yield fp.get(n, r)).max
        for (n <- 1 to fp.getSize; r <- 0 to n) fp.set(n, r, fp.get(n, r) / cp)
        (MatrixExponentiator.expQTtx(finiteInterval.m, finiteInterval.u, finiteInterval.v, finiteInterval.coalRate, finiteInterval.length, fp), cp * cp)
    }
    def updatedCoalRate(i: Int, coalRate: Double): CachedF
  }

  class Nested(interval: BiallelicCoalescentInterval, partial: Int => Double, previousF: CachedF) extends CachedF({
    val F = new F(interval.m)
    for (i <- 0 to interval.k; n <- 1 to previousF.f.getSize; r <- 0 to n)
      F.set(n + interval.k, r + i, F.get(n + interval.k, r + i) + math.max(previousF.f.get(n, r), 0) * partial(i) * HypergeometricPMF(n + interval.k, r + i, interval.k).apply(i))
    (F, previousF.c)
  }, interval) {

    def updatedCoalRate(i: Int, coalRate: Double): Nested = {
      val intervalp = if (interval.coalIndex == i) interval.updatedCoalRate(coalRate) else interval
      val previousFp = previousF.updatedCoalRate(i, coalRate)
      if ((interval ne interval) || (previousF ne previousFp))
        new Nested(intervalp, partial: Int => Double, previousFp)
      else
        this
    }

  }

  class Base(interval: BiallelicCoalescentInterval, f: F) extends CachedF((f, 1), interval) {

    def this(interval: BiallelicCoalescentInterval, partial: Int => Double) = this(interval, {
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
