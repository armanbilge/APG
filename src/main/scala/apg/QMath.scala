package apg

import cz.adamh.utils.NativeUtils

object QMath {

  NativeUtils.loadLibraryFromJar("/apg.jni")

  @native def findOrthogonalVector(N: Int, u: Double, v: Double, gamma: Double): Array[Double]

  def expQTtx(N: Int, u: Double, v: Double, gamma: Double, t: Double, x: F): F =
    new F(N, expQTtx(N, u, v, gamma, t, x.asVectorCopyBase1()))

  @native private[this] def expQTtx(N: Int, u: Double, v: Double, gamma: Double, t: Double, x: Array[Double]): Array[Double]

}
