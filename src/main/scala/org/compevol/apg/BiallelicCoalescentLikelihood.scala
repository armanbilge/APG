package org.compevol.apg

import scala.annotation.tailrec
import scala.collection.LinearSeq

class BiallelicCoalescentLikelihood(val mu: Double, val pi: (Double, Double), val coalescentIntervals: LinearSeq[CoalescentInterval]) extends (LinearSeq[BiallelicSample] => Double) {

  override def apply(samples: LinearSeq[BiallelicSample]): Double = {

    def sorted[T : Ordering](s: Seq[T]) = s.view.zip(s.tail).forall(Function.tupled(implicitly[Ordering[T]].lteq))
    require(sorted(samples))

    case class Interval(length: Double, m: Int, Ne: Double, redProb: Map[Taxon, Double])

    @tailrec
    def recurse(intervals: LinearSeq[CoalescentInterval], samples: LinearSeq[BiallelicSample], nextCoal: Double = 0, t: Double = 0, m: Int = 0, acc: List[Interval] = Nil): List[Interval] = {
      samples match {
        case sample :: samplesTail =>
          val interval :: intervalsTail = intervals
          math.signum(nextCoal compare sample.t) match {
          case -1 => recurse(intervalsTail, samples, nextCoal + interval.length, nextCoal, m, Interval(nextCoal - t, m, interval.Ne, Map()) :: acc)
          case 1 =>
            val mp = m + sample.redProbs.size
            recurse(intervals, samplesTail, nextCoal, sample.t, mp, Interval(sample.t - t, mp, interval.Ne, sample.redProbs) :: acc)
          case 0 =>
            val mp = m + sample.redProbs.size
            recurse(intervalsTail, samplesTail, nextCoal + interval.length, nextCoal, mp, Interval(nextCoal - t, mp, interval.Ne, sample.redProbs) :: acc)
          }
        case Nil => intervals match {
          case interval :: intervalsTail => recurse(intervalsTail, samples, nextCoal + interval.length, nextCoal, m, Interval(nextCoal - t, m, interval.Ne, Map()) :: acc)
          case interval :: Nil => Interval(nextCoal + interval.length, m, interval.Ne, Map()) :: acc
        }
      }
    }

    val intervals = recurse(coalescentIntervals, samples).reverse

  }

}
