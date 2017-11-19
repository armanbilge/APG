package apg


import scala.annotation.tailrec
import scala.collection.LinearSeq

object ExpectedTreeLength {

  def intervalExpectation(theta: Double, length: Double, y0: IndexedSeq[Double], includeSingle: Boolean): Array[Double] = {
    val Q = new UpperTriangularMatrix(y0.length)
    for (i <- 2 until Q.n) {
      Q(0, i) = i
      Q(i, i) = - i * (i - 1) / (2.0 * theta)
      Q(i-1, i) = - Q(i, i)
    }
    if (includeSingle) {
      Q(0, 1) = 1
      val P = Q.eigenVectors
      val Pp = Q.eigenVectors
      Pp(0, 1) = length
      Pp * (P `^-1 *` y0, Q.eigenValues).zipped.map((a, b) => a * math.exp(length * b))
    } else {
      val P = Q.eigenVectors
      if (length.isPosInfinity)
        P * (P `^-1 *` y0).view(0, 2)
      else
        P * (P `^-1 *` y0, Q.eigenValues).zipped.map((a, b) => a * math.exp(length * b))
    }
  }

  def apply(intervals: LinearSeq[SampledCoalescentInterval]): Double = {

    @tailrec
    def recurse(intervals: LinearSeq[SampledCoalescentInterval], includeSingle: List[Boolean], y0: Array[Double] = Array[Double](0)): Double = intervals match {
      case SampledCoalescentInterval(n, length, theta) :: tail =>
        recurse(tail, includeSingle.tail, intervalExpectation(theta, length, new IndexedSeq[Double] {
          override val length = y0.length + n
          override def apply(i: Int) = i match {
            case 0 => y0(0)
            case i if i > n => y0(i - n)
            case _ => 0
          }
        }, includeSingle.head))
      case Nil => y0(0)
    }

    val lastSample = intervals.lastIndexWhere(_.samples > 0)
    val includeSingle = List.tabulate(intervals.length)(_ < lastSample)
    val SampledCoalescentInterval(n, length, theta) :: tail = intervals
    recurse(tail, includeSingle.tail, intervalExpectation(theta, length, new IndexedSeq[Double] {
      override val length = n + 1
      override def apply(i: Int) = if (i == n) 1 else 0
    }, includeSingle.head))
  }

  case class SampledCoalescentInterval(samples: Int, length: Double, theta: Double)

}
