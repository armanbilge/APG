package apg

import cz.adamh.utils.NativeUtils

object Q {

  NativeUtils.loadLibraryFromJar("/apg.jni")

  def findOrthogonalVector(N: Int, u: Double, v: Double, gamma: Double): Array[Double] = {
    val x = new Array[Double](((N + 1) * (N + 1) + N - 1) / 2)
    findOrthogonalVector(N, u, v, gamma, x)
    x
  }

  @native def findOrthogonalVector(N: Int, u: Double, v: Double, gamma: Double, x: Array[Double]): Unit

  def expQTtx(degree: Int, steps: Int, u: Double, v: Double, gamma: Double, t: Double, x: F): F = {
    val y = new Array[Double](x.f.length)
    expQTtx(degree, steps, x.N, x.R, u, v, gamma, t, x.f, y)
    new F(x.N, x.R, y)
  }

  @native private[this] def expQTtx(degree: Int, steps: Int, N: Int, R: Int, u: Double, v: Double, gamma: Double, t: Double, x: Array[Double], y: Array[Double]): Unit

}
