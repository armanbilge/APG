package apg

import apg.BiallelicSiteLikelihood.CachedF
import mcmc.Probability
import snap.likelihood.MatrixExponentiator

import scala.collection.LinearSeq

class BiallelicSiteLikelihood(val piRed: Double, val F: CachedF) extends Probability[Double] {

  require(0 < piRed && piRed < 1.0)

  lazy val evaluate = F.f.get(1, 0) * (1 - piRed) + F.f.get(1, 1) * piRed

  def updatedUV(u: Double, v: Double): BiallelicSiteLikelihood = new BiallelicSiteLikelihood(piRed, F.updatedUV(u, v))
  def updatedCoalRate(i: Int, coalRate: Double): BiallelicSiteLikelihood = new BiallelicSiteLikelihood(piRed, F.updatedCoalRate(i, coalRate))

}

object BiallelicSiteLikelihood {

  def createCachedF(intervals: LinearSeq[BiallelicCoalescentInterval]): CachedF = intervals.tail.foldLeft[CachedF](new Base(intervals.head))((cf, int) => new Nested(int, cf))

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
    def updatedUV(u: Double, v: Double): CachedF
    def updatedCoalRate(i: Int, coalRate: Double): CachedF
  }

  class Nested(interval: BiallelicCoalescentInterval, previousF: CachedF) extends CachedF({
    val F = new F(interval.m)
    for (i <- 0 to interval.k; n <- 1 to previousF.f.getSize; r <- 0 to n)
      F.set(n + interval.k, r + i, F.get(n + interval.k, r + i) + math.max(previousF.f.get(n, r), 0) * interval.partial(i) * HypergeometricPMF(n + interval.k, r + i, interval.k).apply(i))
    (F, previousF.c)
  }, interval) {

    def updatedUV(u: Double, v: Double): Nested = new Nested(interval.updatedUV(u, v), previousF)

    def updatedCoalRate(i: Int, coalRate: Double): Nested = {
      val intervalp = if (interval.coalIndex == i) interval.updatedCoalRate(coalRate) else interval
      val previousFp = previousF.updatedCoalRate(i, coalRate)
      if ((interval ne interval) || (previousF ne previousFp))
        new Nested(intervalp, previousFp)
      else
        this
    }

  }

  class Base(interval: BiallelicCoalescentInterval, f: F) extends CachedF((f, 1), interval) {

    def this(interval: BiallelicCoalescentInterval) = this(interval, {
      val F = new F(interval.m)
      for (r <- 0 to interval.m)
        F.set(interval.m, r, interval.partial(r))
      F
    })

    def updatedUV(u: Double, v: Double): Base = new Base(interval.updatedUV(u, v), f)

    def updatedCoalRate(i: Int, coalRate: Double): Base = {
      if (interval.coalIndex == i)
        new Base(interval.updatedCoalRate(coalRate), f)
      else
        this
    }

  }

}
