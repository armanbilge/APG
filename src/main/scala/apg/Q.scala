package apg

import cz.adamh.utils.NativeUtils
import spire.syntax.cfor.cforRange

object Q {

  private abstract class DoubleArrayUtil[@specialized(Double) T] {
    def fromT(a: Array[T]): Array[Double]
    def toT(a: Array[Double]): Array[T]
  }

  private implicit object doubleImpl extends DoubleArrayUtil[Double] {
    override def fromT(a: Array[Double]): Array[Double] = a
    override def toT(a: Array[Double]): Array[Double] = a
  }

  private implicit object floatImpl extends DoubleArrayUtil[Float] {
    override def fromT(a: Array[Float]): Array[Double] = {
      val b = new Array[Double](a.length)
      cforRange(0 until b.length)(i => b(i) = a(i))
      b
    }
    override def toT(a: Array[Double]): Array[Float] = {
      val b = new Array[Float](a.length)
      cforRange(0 until b.length)(i => b(i) = a(i).toFloat)
      b
    }
  }

  NativeUtils.loadLibraryFromJar("/apg.jni")

  def findOrthogonalVector(N: Int, u: Double, v: Double, gamma: Double): Array[Double] = {
    val x = new Array[Double](((N + 1) * (N + 1) + N - 1) / 2)
    findOrthogonalVector(N, u, v, gamma, x)
    x
  }

  @native def findOrthogonalVector(N: Int, u: Double, v: Double, gamma: Double, x: Array[Double]): Unit

  def expQTtx(degree: Int, steps: Int, u: Double, v: Double, gamma: Double, t: Double, x: F): F = {

    val y = new Array[Double](x.f.length)
    expQTtx(degree, steps, x.N, x.R, u, v, gamma, t, implicitly[DoubleArrayUtil[FP]].fromT(x.f), y)
    new F(x.N, x.R, implicitly[DoubleArrayUtil[FP]].toT(y))

  }

  @native private[this] def expQTtx(degree: Int, steps: Int, N: Int, R: Int, u: Double, v: Double, gamma: Double, t: Double, x: Array[Double], y: Array[Double]): Unit

}
