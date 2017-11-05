package apg

import apg.ExpectedTreeLength.SampledCoalescentInterval
import mcmc.{Operator, OperatorCoercer}
import monocle.Lens
import spire.random.Generator

import scala.annotation.tailrec
import scala.collection.LinearSeq

class CoalescentUpDownOperator[T](val scaleFactor: Double, val Ne: IndexedSeq[Lens[T, Double]], val mu: Traversable[Lens[T, Double]], val intervals: LinearSeq[(Int, Double, Int)])(implicit val rng: Generator) extends Operator[T, Double] {

  require(0 < scaleFactor && scaleFactor <= 1)

  def expectedTreeLength(t: T): Double =
    ExpectedTreeLength(intervals.map {
      case (n, length, i) => SampledCoalescentInterval(n, length, Ne(i).get(t))
    })

  override def apply(t: T): T = {
    val scaleNe = rng.nextDouble(scaleFactor, 1 / scaleFactor)
    val tp = Ne.foldRight(t)(_.modify(scaleNe * _)(_))
    val scaleMu = expectedTreeLength(t) / expectedTreeLength(tp)
    mu.foldRight(tp)(_.modify(scaleMu * _)(_))
  }

  override def hastingsRatio(x: T, y: T): Double = {
    val scaleNe = Ne.head.get(y) / Ne.head.get(x)
    val scaleMu = mu.head.get(y) / mu.head.get(x)
    (Ne.length - 2) * math.log(scaleNe) + mu.size * math.log(scaleMu)
  }

}

object CoalescentUpDownOperator {

  def apply[T, Θ](scaleFactor: Double, Ne: IndexedSeq[Lens[T, Double]], mu: Traversable[Lens[T, Double]], datum: LinearSeq[TimePoint], coalIntervals: LinearSeq[CoalescentInterval[Θ]])(implicit rng: Generator): CoalescentUpDownOperator[T] = {

    @tailrec
    def recurse(intervals: LinearSeq[CoalescentInterval[Θ]], samples: LinearSeq[TimePoint], nextCoal: Double, t: Double = 0, coalIndex: Int = 0, acc: List[(Int, Double, Int)] = Nil): List[(Int, Double, Int)] = if (t.isPosInfinity)
      acc
    else {

      val nextSample = samples.headOption.map(_.t).getOrElse(Double.PositiveInfinity)
      val interval :: intervalsTail = intervals

      val (intervalP, intervalsP, nextCoalP, coalIndexP) = if (t == nextCoal) {
        val nextInterval = intervalsTail.head
        val nextNextCoal = nextCoal + nextInterval.length
        val coalIndexP = coalIndex + 1
        (nextInterval, intervalsTail, nextNextCoal, coalIndexP)
      } else (interval, intervals, nextCoal, coalIndex)

      val (nextSampleP, samplesP, k) = if (t == nextSample) {
        val sample :: samplesTail = samples
        val nextNextSample = samplesTail.headOption.map(_.t).getOrElse(Double.PositiveInfinity)
        val k = sample.k
        (nextNextSample, samplesTail, k)
      } else (nextSample, samples, 0)

      val nextEvent = nextCoalP min nextSampleP

      recurse(intervalsP, samplesP, nextCoalP, nextEvent, coalIndexP, (k, nextEvent - t, coalIndexP) :: acc)

    }

    new CoalescentUpDownOperator[T](scaleFactor, Ne, mu, recurse(coalIntervals, datum, coalIntervals.head.length).reverse)

  }

  implicit def coercer[T]: OperatorCoercer[T, Double, CoalescentUpDownOperator[T]] =
    OperatorCoercer[T, Double, CoalescentUpDownOperator[T]](op => math.log(1 / op.scaleFactor - 1))(x => op => new CoalescentUpDownOperator[T](1 / (math.exp(x) + 1), op.Ne, op.mu, op.intervals)(op.rng))

}
