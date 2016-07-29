package org.compevol.apg

import no.uib.cipr.matrix._
import org.compevol.amh11

import scala.annotation.tailrec
import scala.collection.LinearSeq

class BiallelicCoalescentLikelihood(val mu: Double, val piRed: Double, val coalescentIntervals: LinearSeq[CoalescentInterval]) extends (LinearSeq[BiallelicSample] => Double) {

  require(mu > 0)
  require(0 < piRed && piRed < 1.0)
  val piGreen = 1 - piRed

  val beta = 1 / (1 - piRed * piRed - piGreen * piGreen)

  override def apply(samples: LinearSeq[BiallelicSample]): Double = {

    def sorted[T : Ordering](s: Seq[T]) = s.view.zip(s.tail).forall(Function.tupled(implicitly[Ordering[T]].lteq))
    require(sorted(samples))

    case class Interval(length: Double, m: Int, k: Int, Ne: Double, redCountPMF: Int => Double) {
      val Q = new Q(m, beta * mu * piRed, beta * mu * piGreen, Ne)()
    }

    // TODO Combine recursions

    @tailrec
    def createIntervals(intervals: LinearSeq[CoalescentInterval], samples: LinearSeq[BiallelicSample], nextCoal: Double, t: Double = 0, m: Int = 0, acc: List[Interval] = Nil): List[Interval] = {
      samples match {
        case sample :: samplesTail =>
          val interval :: intervalsTail = intervals
          math.signum(nextCoal compare sample.t) match {
          case -1 => createIntervals(intervalsTail, samples, nextCoal + interval.length, nextCoal, m, Interval(nextCoal - t, m, 0, interval.Ne, Map(m -> 1.0).withDefaultValue(0.0)) :: acc)
          case 1 =>
            val k = sample.redProbs.size
            val mp = m + k
            val nextEvent = nextCoal min samplesTail.headOption.map(_.t).getOrElse(Double.PositiveInfinity)
            createIntervals(intervals, samplesTail, nextCoal, nextEvent, mp, Interval(nextEvent - t, mp, k, interval.Ne, sample.redCountPMF) :: acc)
          case 0 =>
            val k = sample.redProbs.size
            val mp = m + k
            createIntervals(intervalsTail, samplesTail, nextCoal + interval.length, nextCoal, mp, Interval(nextCoal - t, mp, k, interval.Ne, sample.redCountPMF) :: acc)
          }
        case Nil => intervals match {
          case interval :: Nil => acc
          case interval :: intervalsTail => createIntervals(intervalsTail, samples, nextCoal + interval.length, nextCoal, m, Interval(nextCoal - t, m, 0, interval.Ne, Map(m -> 1.0).withDefaultValue(0.0)) :: acc)
        }
      }
    }

    val intervals = createIntervals(coalescentIntervals, samples, coalescentIntervals.head.length).reverse

    @tailrec
    def logLikelihood(intervals: Seq[Interval], x: Option[LAVector] = None): Vector = {
      val interval = intervals.head
      val xp = x match {
        case None => new LAVector(interval.m)()({ (n, r) =>
          if (n == interval.m) interval.redCountPMF(r) else 0
        })
        case Some(z) => (0 to interval.k).foldLeft(new LAVector(interval.m)()()) { (v, i) =>
          val p = interval.redCountPMF(i)
          if (p > 0)
            z.index.foreach { (n, r) =>
              v(n + interval.k, r + i) = v(n + interval.k, r + i) + z(n, r) * p
            }
          v
        }
      }
      if (interval.length.isInfinity) {
        val n = xp.size
        val evd = new EVD(n, false, true).factor(interval.Q)
        val i = evd.getRealEigenvalues.map(math.abs).zipWithIndex.minBy(_._1)._2
        val eD = new DenseMatrix(n, n)
        eD.set(i, i, 1)
        evd.getRightEigenvectors.mult(eD, new DenseMatrix(n, n)).mult(DenseLU.factorize(evd.getRightEigenvectors).solve(Matrices.identity(n)), new DenseMatrix(n, n)).mult(xp, new DenseVector(n))
      } else {
        val y = new LAVector(interval.m)()()
        y.add(amh11.expmv(interval.length, interval.Q, xp, approx = true))
        logLikelihood(intervals.tail, Some(y))
      }
    }

    val y = logLikelihood(intervals)
    y.get(0) * piRed + y.get(1) * piGreen

  }

}
