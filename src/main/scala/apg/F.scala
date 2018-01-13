package apg

class F(val N: Int, val f: Array[Double]) {

  def apply(n: Int, r: Int): Double = f(n*(n+1)/2-1+r)

  def update(n: Int, r: Int, x: Double): Unit = f(n*(n+1)/2-1+r) = x

}

object F {

  def apply(N: Int): F = new F(N, new Array((N+1)*(N+2)/2-1))

}
