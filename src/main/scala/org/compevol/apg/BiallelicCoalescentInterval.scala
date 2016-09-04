package org.compevol.apg

sealed trait BiallelicCoalescentInterval {
  val m: Int
  val k: Int
}

object BiallelicCoalescentInterval {

  def apply(length: Double, m: Int, k: Int, u: Double, v: Double, coalRate: Double) = if (length.isInfinity)
    InfiniteBiallelicCoalescentInterval(m, k, u, v, coalRate)
  else
    FiniteBiallelicCoalescentInterval(length, m, k, u, v, coalRate)

}

case class FiniteBiallelicCoalescentInterval(length: Double, m: Int, k: Int, u: Double, v: Double, coalRate: Double) extends BiallelicCoalescentInterval

case class InfiniteBiallelicCoalescentInterval(m: Int, k: Int, u: Double, v: Double, coalRate: Double) extends BiallelicCoalescentInterval {

  val pi = {
    val pihat = new Q(m, u, v, coalRate).findOrthogonalVector(false)
    val z = pihat(1) + pihat(2)
    pihat.map(_ / z)
  }

}
