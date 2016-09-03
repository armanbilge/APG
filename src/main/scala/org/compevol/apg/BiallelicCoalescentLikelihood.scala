package org.compevol.apg

import org.apache.commons.math3.distribution.HypergeometricDistribution
import snap.likelihood.MatrixExponentiator

import scala.annotation.tailrec
import scala.collection.LinearSeq

class BiallelicCoalescentLikelihood(val mu: Double, val piRed: Double, val coalescentIntervals: LinearSeq[CoalescentInterval]) extends (LinearSeq[TimePoint] => Double) {

  require(mu > 0)
  require(0 < piRed && piRed < 1.0)
  val piGreen = 1 - piRed

  val beta = 1 / (1 - piRed * piRed - piGreen * piGreen)

  val u = beta * mu * piRed
  val v = beta * mu * piGreen

  override def apply(samples: LinearSeq[TimePoint]): Double = {

    def sorted[T : Ordering](s: Seq[T]) = s.view.zip(s.tail).forall(Function.tupled(implicitly[Ordering[T]].lteq))
    require(sorted(samples))

    case class Interval(length: Double, m: Int, k: Int, coalRate: Double, redCountPartial: Int => Double)

    // TODO Combine recursions

    @tailrec
    def createIntervals(intervals: LinearSeq[CoalescentInterval], samples: LinearSeq[TimePoint], nextCoal: Double, t: Double = 0, m: Int = 0, acc: List[Interval] = Nil): List[Interval] = {
      samples match {
        case sample :: samplesTail =>
          val interval :: intervalsTail = intervals
          math.signum(nextCoal compare sample.t) match {
          case -1 => createIntervals(intervalsTail, samples, nextCoal + interval.length, nextCoal, m, Interval(nextCoal - t, m, 0, 1 / interval.Ne, Map(m -> 1.0).withDefaultValue(0.0)) :: acc)
          case 1 =>
            val k = sample.k
            val mp = m + k
            val nextEvent = nextCoal min samplesTail.headOption.map(_.t).getOrElse(Double.PositiveInfinity)
            createIntervals(intervals, samplesTail, nextCoal, nextEvent, mp, Interval(nextEvent - t, mp, k, 1 / interval.Ne, sample.redCountPartial) :: acc)
          case 0 =>
            val k = sample.k
            val mp = m + k
            createIntervals(intervalsTail, samplesTail, nextCoal + interval.length, nextCoal, mp, Interval(nextCoal - t, mp, k, 1 / interval.Ne, sample.redCountPartial) :: acc)
          }
        case Nil => intervals match {
          case interval :: Nil => acc
          case interval :: intervalsTail => createIntervals(intervalsTail, samples, nextCoal + interval.length, nextCoal, m, Interval(nextCoal - t, m, 0, 1 / interval.Ne, Map(m -> 1.0).withDefaultValue(0.0)) :: acc)
        }
      }
    }

    val intervals = createIntervals(coalescentIntervals, samples, coalescentIntervals.head.length).reverse

    @tailrec
    def logLikelihood(intervals: Seq[Interval], x: Option[F] = None): F = {
      val interval = intervals.head
      val xp = x match {
        case None =>
          val xp = new F(interval.m)
          for (r <- 0 to interval.m)
            xp.set(interval.m, r, interval.redCountPartial(r))
          xp
        case Some(z) =>
          val xp = new F(interval.m)
          for (i <- 0 to interval.k; n <- 1 to z.getSize; r <- 0 to n)
            xp.set(n + interval.k, r + i, xp.get(n + interval.k, r + i) + z.get(n, r) * interval.redCountPartial(i) * new HypergeometricDistribution(null, n + interval.k, r + i, interval.k).probability(i))
          xp
        }
      if (interval.length.isInfinity) {
        val z = new Q(interval.m, u, v, interval.coalRate).findOrthogonalVector(false)
        val l = (xp.asVectorCopyBase1(), z).zipped.map(_ * _).sum / (z(1) + z(2))
        val y = new F(1)
        y.set(1, 0, l)
        y.set(1, 1, l)
        y
      } else {
        logLikelihood(intervals.tail, Some(MatrixExponentiator.expQTtx(interval.m, u, v, interval.coalRate, interval.length, xp)))
      }
    }

    val y = logLikelihood(intervals)
    math.log(y.get(1, 0) * piGreen + y.get(1, 1) * piRed)

  }

}
