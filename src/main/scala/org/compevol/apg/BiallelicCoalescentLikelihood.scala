package org.compevol.apg

import no.uib.cipr.matrix.sparse.CompDiagMatrix
import no.uib.cipr.matrix._
import org.compevol.amh11

import scala.annotation.tailrec
import scala.collection.LinearSeq

class BiallelicCoalescentLikelihood(val mu: Double, val pi: (Double, Double), val coalescentIntervals: LinearSeq[CoalescentInterval]) extends (LinearSeq[BiallelicSample] => Double) {

  require(mu > 0)
  require(pi._1 > 0 && pi._2 > 0 && pi._1 + pi._2 == 1.0)

  val beta = 1 / (1 - pi._1 * pi._1 - pi._2 * pi._2)

  override def apply(samples: LinearSeq[BiallelicSample]): Double = {

    def sorted[T : Ordering](s: Seq[T]) = s.view.zip(s.tail).forall(Function.tupled(implicitly[Ordering[T]].lteq))
    require(sorted(samples))

    case class Interval(length: Double, m: Int, Ne: Double, redCountPMF: Int => Double) {
      val Q = new Q(m, beta * mu * pi._1, beta * mu * pi._2, Ne)()
    }

    // TODO Combine recursions

    @tailrec
    def createIntervals(intervals: LinearSeq[CoalescentInterval], samples: LinearSeq[BiallelicSample], nextCoal: Double, t: Double = 0, m: Int = 0, acc: List[Interval] = Nil): List[Interval] = {
      samples match {
        case sample :: samplesTail =>
          val interval :: intervalsTail = intervals
          math.signum(nextCoal compare sample.t) match {
          case -1 => createIntervals(intervalsTail, samples, nextCoal + interval.length, nextCoal, m, Interval(nextCoal - t, m, interval.Ne, Map(m -> 1.0).withDefaultValue(0.0)) :: acc)
          case 1 =>
            val mp = m + sample.redProbs.size
            val nextEvent = nextCoal min samplesTail.headOption.map(_.t).getOrElse(Double.PositiveInfinity)
            createIntervals(intervals, samplesTail, nextCoal, nextEvent, mp, Interval(nextEvent - t, mp, interval.Ne, sample.redCountPMF) :: acc)
          case 0 =>
            val mp = m + sample.redProbs.size
            createIntervals(intervalsTail, samplesTail, nextCoal + interval.length, nextCoal, mp, Interval(nextCoal - t, mp, interval.Ne, sample.redCountPMF) :: acc)
          }
        case Nil => intervals match {
          case interval :: Nil => acc
          case interval :: intervalsTail => createIntervals(intervalsTail, samples, nextCoal + interval.length, nextCoal, m, Interval(nextCoal - t, m, interval.Ne, Map(m -> 1.0).withDefaultValue(0.0)) :: acc)
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
        case Some(y) => (0 to interval.m).foldLeft(new LAVector(interval.m)()()) { (v, i) =>
          val p = interval.redCountPMF(i)
          if (p > 0)
            y.index.foreach { (n, r) =>
              v(n + interval.m, r + i) = v(n + interval.m, r + i) + y(n, r) * p
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
        y.add(amh11.expmv(interval.length, interval.Q, xp))
        logLikelihood(intervals.tail, Some(y))
      }
    }

    val y = logLikelihood(intervals)
    math.log(y.get(0) * pi._1 + y.get(1) * pi._2)

  }

}
