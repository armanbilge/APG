package apg

import org.apache.commons.math3.distribution.{BinomialDistribution, HypergeometricDistribution}
import org.apache.commons.math3.exception.NotStrictlyPositiveException

object HypergeometricPMF extends ((Int, Int, Int, Int) => Double) {

  def apply(N: Int, K: Int, n: Int, k: Int): Double = cache(N)(K)(n)(k)

  private val cache: Array[Array[Array[Array[Double]]]] = Array.tabulate(100 + 1) { N =>
    Array.tabulate(N+1, N+1) { (K, n) =>
      Array.tabulate(n+1)(pmf(N, K, n, _))
    }
  }

  private def pmf(N: Int, K: Int, n: Int, k: Int): Double = {
    try {
      new HypergeometricDistribution(null, N, K, n).probability(k) match {
        case nan if nan.isNaN =>
          val p = n.toDouble / N
          val p1 = new BinomialDistribution(null, K, p).logProbability(k)
          val p2 = new BinomialDistribution(null, N - K, p).logProbability(n - k)
          val p3 = new BinomialDistribution(null, N, p).logProbability(n)
          math.exp(p1 + p2 - p3)
        case p => p
      }
    } catch {
      case _: NotStrictlyPositiveException => Double.NaN
    }
  }

}
