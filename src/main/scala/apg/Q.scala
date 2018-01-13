package apg

import cz.adamh.utils.NativeUtils

object Q {

  NativeUtils.loadLibraryFromJar("/apg.jni")

  @native def findOrthogonalVector(N: Int, u: Double, v: Double, gamma: Double): Array[Double]

  def expQTtx(degree: Int, steps: Int, N: Int, u: Double, v: Double, gamma: Double, t: Double, x: F): F =
    new F(N, expQTtx(degree, steps, N, u, v, gamma, t, x.f))

  @native private[this] def expQTtx(degree: Int, steps: Int, N: Int, u: Double, v: Double, gamma: Double, t: Double, x: Array[Double]): Array[Double]

}
