package org.compevol.apg

import scala.annotation.tailrec
import scala.collection.LinearSeq

class BiallelicCoalescentLikelihood(val data: IndexedSeq[LinearSeq[TimePoint]]) extends ((Double, Double, LinearSeq[CoalescentInterval]) => Double) {

  private[this] val transformedData = data.map(_.map(_.redCountPartial)).par

  private[this] val cachedHypergeometricDistribution = {
    val I = data.head.map(_.k).sum
    Array.tabulate(I+1) { N =>
      Array.tabulate(N+1, N+1) { (K, n) =>
        Array.tabulate(n+1)(HypergeometricPMF(N, K, n))
      }
    }
  }

  private[this] def hypergeometricDistribution(N: Int, K: Int, n: Int)(k: Int) = cachedHypergeometricDistribution(N)(K)(n)(k)

  override def apply(mu: Double, piRed: Double, coalIntervals: LinearSeq[CoalescentInterval]): Double = {

    require(mu > 0)
    val piGreen = 1 - piRed
    val beta = 1 / (1 - piRed * piRed - piGreen * piGreen)

    val u = beta * mu * piRed
    val v = beta * mu * piGreen

    @tailrec
    def createIntervals(intervals: LinearSeq[CoalescentInterval], samples: LinearSeq[TimePoint], nextCoal: Double, t: Double = 0, m: Int = 0, acc: List[BiallelicCoalescentInterval] = Nil): List[BiallelicCoalescentInterval] = {
      samples match {
        case sample :: samplesTail =>
          val interval :: intervalsTail = intervals
          math.signum(nextCoal compare sample.t) match {
            case -1 => createIntervals(intervalsTail, samples, nextCoal + interval.length, nextCoal, m, BiallelicCoalescentInterval(nextCoal - t, m, 0, u, v, 1 / interval.Ne) :: acc)
            case 1 =>
              val k = sample.k
              val mp = m + k
              val nextEvent = nextCoal min samplesTail.headOption.map(_.t).getOrElse(Double.PositiveInfinity)
              createIntervals(intervals, samplesTail, nextCoal, nextEvent, mp, BiallelicCoalescentInterval(nextEvent - t, mp, k, u, v, 1 / interval.Ne) :: acc)
            case 0 =>
              val k = sample.k
              val mp = m + k
              createIntervals(intervalsTail, samplesTail, nextCoal + interval.length, nextCoal, mp, BiallelicCoalescentInterval(nextCoal - t, mp, k, u, v, 1 / interval.Ne) :: acc)
          }
        case Nil => intervals match {
          case interval :: Nil => acc
          case interval :: intervalsTail => createIntervals(intervalsTail, samples, nextCoal + interval.length, nextCoal, m, BiallelicCoalescentInterval(nextCoal - t, m, 0, u, v, 1 / interval.Ne) :: acc)
        }
      }
    }

    val intervals = createIntervals(coalIntervals, data.head, coalIntervals.head.length).reverse
    val siteProb = new BiallelicSiteProbability(piRed, hypergeometricDistribution)

    transformedData.map(partials => siteProb(intervals, partials)).sum

  }

}
