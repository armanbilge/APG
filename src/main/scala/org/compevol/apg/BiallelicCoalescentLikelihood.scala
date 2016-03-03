package org.compevol.apg

import scala.annotation.tailrec
import scala.collection.LinearSeq

class BiallelicCoalescentLikelihood(val mu: Double, val pi: (Double, Double), val coalescentIntervals: LinearSeq[CoalescentInterval]) extends (LinearSeq[BiallelicSample] => Double) {

  override def apply(samples: LinearSeq[BiallelicSample]): Double = {

    case class Interval(length: Double, m: Int, Ne: Double, redProb: Map[Taxon, Double])

    // TODO Can't I make this any nicer?
    @tailrec
    def recurse(intervals: LinearSeq[CoalescentInterval], samples: LinearSeq[BiallelicSample], coalT: Double, t: Double = 0, m: Int = 0, acc: List[Interval] = Nil): List[Interval] = {
      if (samples.isEmpty) {
        if (intervals.isEmpty)
          acc
        else {
          val interval = intervals.head
          val intervalsp = intervals.tail
          recurse(intervalsp, samples, coalT + intervalsp.head.length, coalT, m, Interval(coalT - t, m, interval.Ne, Map()) :: acc)
        }
      } else {
        val interval = intervals.head
        val sample = samples.head
        val c = coalT compare sample.t
        if (c < 0) {
          val intervalsp = intervals.tail
          recurse(intervalsp, samples, coalT + intervalsp.head.length, coalT, m, Interval(coalT - t, m, interval.Ne, Map()) :: acc)
        } else if (c > 0) {
          val mp = m + sample.redProbs.size
          recurse(intervals, samples.tail, coalT, sample.t, mp, Interval(sample.t - t, mp, interval.Ne, sample.redProbs) :: acc)
        } else {
          val intervalsp = intervals.tail
          val mp = m + sample.redProbs.size
          recurse(intervalsp, samples.tail, coalT + intervalsp.head.length, coalT, mp, Interval(coalT - t, mp, interval.Ne, sample.redProbs) :: acc)
        }
      }
    }

    val intervals = recurse(coalescentIntervals, samples, coalescentIntervals.head.length)

  }

}
