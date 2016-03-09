package org.compevol.apg

import org.compevol.amh11

import scala.annotation.tailrec
import scala.collection.LinearSeq

class BiallelicCoalescentLikelihood(val mu: Double, val pi: (Double, Double), val coalescentIntervals: LinearSeq[CoalescentInterval]) extends (LinearSeq[BiallelicSample] => Double) {

  require(mu > 0)
  require(pi._1 > 0 && pi._2 > 0 && pi._1 + pi._2 == 1.0)

  override def apply(samples: LinearSeq[BiallelicSample]): Double = {

    def sorted[T : Ordering](s: Seq[T]) = s.view.zip(s.tail).forall(Function.tupled(implicitly[Ordering[T]].lteq))
    require(sorted(samples))

    case class Interval(length: Double, m: Int, Ne: Double, redCountPMF: Int => Double) {
      val Q = new Q(m, mu * pi._1, mu * pi._2, Ne)()
    }

    @tailrec
    def recurse(intervals: LinearSeq[CoalescentInterval], samples: LinearSeq[BiallelicSample], nextCoal: Double, t: Double = 0, m: Int = 0, acc: List[Interval] = Nil): List[Interval] = {
      samples match {
        case sample :: samplesTail =>
          val interval :: intervalsTail = intervals
          math.signum(nextCoal compare sample.t) match {
          case -1 => recurse(intervalsTail, samples, nextCoal + interval.length, nextCoal, m, Interval(nextCoal - t, m, interval.Ne, Map(m -> 1.0).withDefaultValue(0.0)) :: acc)
          case 1 =>
            val mp = m + sample.redProbs.size
            recurse(intervals, samplesTail, nextCoal, sample.t, mp, Interval((nextCoal min samplesTail.headOption.map(_.t).getOrElse(Double.PositiveInfinity)) - t, mp, interval.Ne, sample.redCountPMF) :: acc)
          case 0 =>
            val mp = m + sample.redProbs.size
            recurse(intervalsTail, samplesTail, nextCoal + interval.length, nextCoal, mp, Interval(nextCoal - t, mp, interval.Ne, sample.redCountPMF) :: acc)
          }
        case Nil => intervals match {
          case interval :: Nil => Interval(nextCoal + interval.length, m, interval.Ne, Map()) :: acc
          case interval :: intervalsTail => recurse(intervalsTail, samples, nextCoal + interval.length, nextCoal, m, Interval(nextCoal - t, m, interval.Ne, Map(m -> 1.0).withDefaultValue(0.0)) :: acc)
        }
      }
    }

    val intervals = recurse(coalescentIntervals, samples, coalescentIntervals.head.length).reverse

    val initial = intervals.head
    val x = new LAVector(initial.m)()({ (n, r) =>
      if (n == initial.m) initial.redCountPMF(r) else 0
    })
    val y = intervals.foldRight(x) { (interval, x) =>
      val xp = (0 to interval.m).foldLeft(new LAVector(interval.m)()()) { (v, i) =>
        val p = interval.redCountPMF(i)
        if (p > 0)
          x.index.foreach { (n, r) =>
            v(n + interval.m, r + i) = v(n + interval.m, r + i) + x(n, r) * p
          }
        v
      }
      val y = new LAVector(interval.m)()()
      y.add(amh11.expmv(interval.length, interval.Q, xp))
      y
    }

    math.log(y(1, 0) * pi._1 + y(1, 1) * pi._2)

  }

}
