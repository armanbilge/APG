package apg

import apg.BiallelicSiteLikelihood.IntervalIntegrator
import mcmc.Probability

class BiallelicSiteLikelihood(val intervalIntegrator: IntervalIntegrator) extends Probability[Double] {

  override lazy val evaluate: Double = intervalIntegrator.probability

  def updateIntegrators(integrators: PartialFunction[Int, Either[F => F, F => Double]]) =
    new BiallelicSiteLikelihood(intervalIntegrator.updateIntegrators(integrators))

}

object BiallelicSiteLikelihood {

  def apply(intervals: Seq[BiallelicCoalescentInterval], partials: Seq[IndexedSeq[Double]], integrators: Seq[Either[F => F, F => Double]]): BiallelicSiteLikelihood = {
    val intervalsIterator = intervals.iterator
    val nextIntervalsIterator = intervals.iterator.drop(1)
    val partialsIterator = partials.iterator
    val integratorsIterator = integrators.iterator
    def handleIntervals() = {
      val interval = intervalsIterator.next()
      val nextInterval = if (nextIntervalsIterator.hasNext) Some(nextIntervalsIterator.next()) else None
      (interval, nextInterval.exists(_.coalIndex != interval.coalIndex))
    }
    val (interval, cache) = handleIntervals()
    var intervalIntegrator: IntervalIntegrator = if (cache) new Base(interval.m, partialsIterator.next(), integratorsIterator.next()) with Cached else new Base(interval.m, partialsIterator.next(), integratorsIterator.next())
    while (intervalsIterator.hasNext) {
      val (interval, cache) = handleIntervals()
      intervalIntegrator = if (cache)
        new Nested(intervalIntegrator, interval.m, interval.k, if (interval.k > 0) partialsIterator.next() else IndexedSeq.empty[Double], integratorsIterator.next()) with Cached
      else
        new Nested(intervalIntegrator, interval.m, interval.k, if (interval.k > 0) partialsIterator.next() else IndexedSeq.empty[Double], integratorsIterator.next())
    }
    new BiallelicSiteLikelihood(intervalIntegrator)
  }

  abstract class IntervalIntegrator(val integrator: Either[F => F, F => Double], val index: Int) {

    def initial: F

    def probability: Double = integrator match {
      case Right(f) => f(initial)
    }

    def integrated: F = integrator match {
      case Left(f) => f(initial)
    }

    def updateIntegrators(integrators: PartialFunction[Int, Either[F => F, F => Double]]): IntervalIntegrator

  }

  trait Cached extends IntervalIntegrator {
    override lazy val probability: Double = super.probability
    override lazy val integrated: F = super.integrated
  }

  class Nested(previous: IntervalIntegrator, val m: Int, val k: Int, val partial: IndexedSeq[Double], _integrator: Either[F => F, F => Double]) extends IntervalIntegrator(_integrator, previous.index + 1) {

    override def initial: F = if (k > 0) {
      val previousF = previous.integrated
      val initial = F(m, previousF.R + partial.length - 1)
      import spire.syntax.cfor._
      var i = initial.index(k+1, 0)
      var j = 0
      cforRange((k+1) to initial.N) { n =>
        val R = math.min(n-k, previousF.R)
        cforRange(0 until partial.length) { l =>
          cforRange(0 to R) { r =>
            initial.f(i) += previousF.f(j) * partial(l) * HypergeometricPMF(n, r + l, k, l)
            i += 1
            j += 1
          }
          if (l < partial.length - 1) {
            i -= R
            j -= R + 1
          } else {
            i += math.min(n, initial.R) - (R + partial.length - 1)
          }
        }
      }
      initial
    } else previous.integrated

    override def updateIntegrators(integrators: PartialFunction[Int, Either[F => F, F => Double]]): IntervalIntegrator = {
      val previous = this.previous.updateIntegrators(integrators)
      val integrator = integrators.applyOrElse(index, (_: Int) => this.integrator)
      if ((previous ne this.previous) || (integrator ne this.integrator))
        if (this.isInstanceOf[Cached])
          new Nested(previous, m, k, partial, integrator) with Cached
        else
          new Nested(previous, m, k, partial, integrator)
      else
        this
    }

  }

  class Base(val initial: F, _integrator: Either[F => F, F => Double]) extends IntervalIntegrator(_integrator, 0) {

    def this(m: Int, partial: IndexedSeq[Double], integrator: Either[F => F, F => Double]) = this({
      val f = F(m, partial.length - 1)
      for (r <- 0 to f.R)
        f(m, r) = partial(r)
      f
    }, integrator)

    override def updateIntegrators(integrators: PartialFunction[Int, Either[F => F, F => Double]]): Base = integrators.andThen { int =>
      if (Base.this.isInstanceOf[Cached])
        new Base(initial, int) with Cached
      else
        new Base(initial, int)
    }.applyOrElse(index, (_: Int) => this)

  }

}
