package org.compevol.apg

import org.jtransforms.fft.DoubleFFT_1D
import spire.algebra.{Field, Trig}
import spire.math.Complex
import spire.std.double._

class PoissonBinomialDistribution(val p: Seq[Double]) extends (Int => Double) {

  require(p.forall(pi => 0 <= pi && pi <= 1))
  private[this] val N = p.length
  private[this] val c = p.count(_ == 1)
  private[this] val pp = p.filterNot(_ == 0).filterNot(_ == 1)
  private[this] val Np = pp.length
  private[this] val alpha = pp.product
  private[this] val s = pp.map(pk => -(1 - pk) / pk)
  private[this] val r = Traversable.tabulate(Np + 1) { n =>
    val z = Trig[Complex[Double]].exp(Complex.i[Double] * 2 * Trig[Complex[Double]].pi * n / (Np + 1))
    Field[Complex[Double]].prod(s.map(z - _))
  }.flatMap(z => Traversable(z.real, z.imag)).toArray
  private[this] val fft = new DoubleFFT_1D(Np + 1)
  fft.complexForward(r)
  private[this] val mass = r.grouped(2).map(_(0) * alpha / (Np + 1)).toVector

  def apply(i: Int): Double = {
    require(0 <= i && i <= N)
    if (i < c || i > Np + c)
      0
    else
      mass(i - c)
  }

}
