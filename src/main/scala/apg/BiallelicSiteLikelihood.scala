package apg

import apg.BiallelicSiteLikelihood.CachedF
import mcmc.Probability

import scala.collection.LinearSeq

class BiallelicSiteLikelihood(val piRed: Double, infiniteInterval: => InfiniteBiallelicCoalescentInterval, val partials: LinearSeq[Int => Double], val F: CachedF) extends Probability[Double] with Serializable {

  require(0 < piRed && piRed < 1.0)

  lazy val evaluate: Double = NativeLib.siteLikelihood(infiniteInterval.m, F.f, infiniteInterval.pi)

  def updatedCoalRate(i: Int, coalRate: Double, infiniteInterval: => InfiniteBiallelicCoalescentInterval): BiallelicSiteLikelihood = new BiallelicSiteLikelihood(piRed, infiniteInterval, partials, F.updatedCoalRate(i, coalRate))

}

object BiallelicSiteLikelihood {

  def apply(piRed: Double, infiniteInterval: => InfiniteBiallelicCoalescentInterval, intervals: LinearSeq[BiallelicCoalescentInterval], partials: LinearSeq[Int => Double]): BiallelicSiteLikelihood =
    new BiallelicSiteLikelihood(piRed, infiniteInterval, partials, createCachedF(intervals, partials))

  def createCachedF(intervals: LinearSeq[BiallelicCoalescentInterval], partials: LinearSeq[Int => Double]): CachedF = {
    val intervalsIterator = intervals.iterator
    val partialsIterator = partials.iterator
    val interval = intervalsIterator.next()
    var F: CachedF = Base(interval, NativeLib.copyToNative(Array.tabulate(interval.k + 1)(partialsIterator.next())))
    while (intervalsIterator.hasNext) {
      val interval = intervalsIterator.next()
      F = new Nested(interval, if (interval.k > 0) NativeLib.copyToNative(Array.tabulate(interval.k + 1)(partialsIterator.next())) else null, F)
    }
    F
  }

  abstract class CachedF(fp: => NativePointer, val interval: BiallelicCoalescentInterval) extends Serializable {
    lazy val f: NativePointer = interval match {
      case interval: FiniteBiallelicCoalescentInterval => NativeLib.expQTtx(interval.m, interval.u, interval.v, interval.coalRate, interval.length, fp)
      case _: InfiniteBiallelicCoalescentInterval => fp
    }
    def updatedCoalRate(i: Int, coalRate: Double): CachedF
  }

  class Nested(interval: BiallelicCoalescentInterval, partial: NativePointer, previousF: CachedF)
    extends CachedF(if (interval.k > 0) NativeLib.fWithPartial(previousF.interval.m, previousF.f, interval.k, partial) else previousF.f, interval) with Serializable {

    def updatedCoalRate(i: Int, coalRate: Double): Nested = {
      val intervalp = if (interval.coalIndex == i) interval.updatedCoalRate(coalRate) else interval
      val previousFp = previousF.updatedCoalRate(i, coalRate)
      if ((interval ne intervalp) || (previousF ne previousFp))
        new Nested(intervalp, partial, previousFp)
      else
        this
    }

  }

  class Base(interval: BiallelicCoalescentInterval, baseF: => NativePointer) extends CachedF(baseF, interval) with Serializable {

    def updatedCoalRate(i: Int, coalRate: Double): Base = {
      if (interval.coalIndex == i)
        new Base(interval.updatedCoalRate(coalRate), baseF)
      else
        this
    }

  }

  object Base {
    def apply(interval: BiallelicCoalescentInterval, partial: NativePointer) =
      new Base(interval, NativeLib.fFromPartial(interval.k, partial))
  }

}
