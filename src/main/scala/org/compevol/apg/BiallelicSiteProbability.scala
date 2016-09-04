package org.compevol.apg

import org.apache.commons.math3.distribution.HypergeometricDistribution
import snap.likelihood.MatrixExponentiator

import scala.annotation.tailrec
import scala.collection.LinearSeq

class BiallelicSiteProbability(val piRed: Double, hypergeometricDistribution: (Int, Int, Int) => (Int => Double) = (N, K, n) => new HypergeometricDistribution(null, N, K, n).probability) extends ((LinearSeq[BiallelicCoalescentInterval], LinearSeq[Int => Double]) => Double) {

  require(0 < piRed && piRed < 1.0)
  val piGreen = 1 - piRed

  def apply(intervals: LinearSeq[BiallelicCoalescentInterval], partials: LinearSeq[Int => Double]): Double = {

    @tailrec
    def logLikelihood(intervals: LinearSeq[BiallelicCoalescentInterval], partials: LinearSeq[Int => Double], x: Option[F] = None): F = {
      val interval = intervals.head
      val (partial, partialsTail) = if (interval.k > 0)
        (partials.head, partials.tail)
      else
        (Map(interval.m -> 1.0).withDefaultValue(0.0), partials)
      val xp = x match {
        case None =>
          val xp = new F(interval.m)
          for (r <- 0 to interval.m)
            xp.set(interval.m, r, partial(r))
          xp
        case Some(z) =>
          val xp = new F(interval.m)
          for (i <- 0 to interval.k; n <- 1 to z.getSize; r <- 0 to n)
            xp.set(n + interval.k, r + i, xp.get(n + interval.k, r + i) + z.get(n, r) * partial(i) * hypergeometricDistribution(n + interval.k, r + i, interval.k).apply(i))
          xp
      }
      interval match {
        case infiniteInterval: InfiniteBiallelicCoalescentInterval =>
          val l = (xp.asVectorCopyBase1(), infiniteInterval.pi).zipped.map(_ * _).sum
          val y = new F(1)
          y.set(1, 0, l)
          y.set(1, 1, l)
          y
        case finiteInterval: FiniteBiallelicCoalescentInterval =>
          logLikelihood(intervals.tail, partialsTail, Some(MatrixExponentiator.expQTtx(finiteInterval.m, finiteInterval.u, finiteInterval.v, finiteInterval.coalRate, finiteInterval.length, xp)))
      }
    }

    val y = logLikelihood(intervals, partials)
    math.log(y.get(1, 0) * piGreen + y.get(1, 1) * piRed)

  }

}
