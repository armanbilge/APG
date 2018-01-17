package apg

import cz.adamh.utils.NativeUtils

object Q {

  NativeUtils.loadLibraryFromJar("/apg.jni")

  def findOrthogonalVector(N: Int, u: Double, v: Double, gamma: Double): Array[Float] = {
    val x = new Array[Float](((N + 1) * (N + 1) + N - 1) / 2)
    findOrthogonalVector(N, u, v, gamma, x)
    x
  }

  @native def findOrthogonalVector(N: Int, u: Double, v: Double, gamma: Double, x: Array[Float]): Unit

  def expQTtx(degree: Int, steps: Int, N: Int, u: Double, v: Double, gamma: Double, t: Double, x: F): F = {
    val y = new Array[Float](x.f.length)
    expQTtx(degree, steps, N, u, v, gamma, t, x.f, y)
    new F(N, y)
  }


  @native private[this] def expQTtx(degree: Int, steps: Int, N: Int, u: Double, v: Double, gamma: Double, t: Double, x: Array[Float], y: Array[Float]): Unit

}
