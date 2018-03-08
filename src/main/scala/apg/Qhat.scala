package apg

class Qhat(val N: Int, val u: Double, val v: Double, val theta: Double) extends UpperTriangularMatrix {

  override val n = (N+1) * (N+2) / 2 - 1

  def unpack(i: Int) = {
    val n = (math.sqrt(8*i + 9).toInt - 1) / 2
    val r = i - (n * (n+1) / 2 - 1)
    (n, r)
  }

  override def apply(i: Int, j: Int): Double = {

    val (ni, ri) = unpack(i)
    val (nj, rj) = unpack(j)

    if (i == j)
      - ni * (ni-1) / theta - (ni-ri) * v - ri * u
    else if (ni > 1 && ni == nj && ri == 0 && rj == 1)
      ni * v
    else if (ni+1 == nj && ri == rj && rj < nj)
      (nj - 1 - rj) * nj / theta
    else if (ni+1 == nj && ri+1 == rj && rj > 1)
      (rj - 1) * nj / theta
    else
      0.0
  }

}
