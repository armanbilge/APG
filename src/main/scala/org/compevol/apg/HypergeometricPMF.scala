package org.compevol.apg

import org.apache.commons.math3.distribution.{BinomialDistribution, HypergeometricDistribution}
import org.apache.commons.math3.exception.NotStrictlyPositiveException

object HypergeometricPMF extends ((Int, Int, Int) => (Int => Double)) {

  override def apply(N: Int, K: Int, n: Int): (Int => Double) = { k =>
    try {
      new HypergeometricDistribution(N, K, n).probability(k) match {
        case nan if nan.isNaN =>
          val p = n.toDouble / N
          val p1 = new BinomialDistribution(K, p).logProbability(k)
          val p2 = new BinomialDistribution(N - K, p).logProbability(n - k)
          val p3 = new BinomialDistribution(N, p).logProbability(n)
          math.exp(p1 + p2 - p3)
        case p => p
      }
    } catch {
      case _: NotStrictlyPositiveException => Double.NaN
    }
  }

}
