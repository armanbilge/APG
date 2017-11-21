package apg

sealed trait BiallelicCoalescentInterval {
  val m: Int
  val k: Int
  val coalIndex: Int
  def updatedCoalRate(coalRate: Double): BiallelicCoalescentInterval
}

object BiallelicCoalescentInterval {

  def apply(length: Double, m: Int, k: Int, u: Double, v: Double, coalRate: Double, coalIndex: Int) = if (length.isInfinity)
    InfiniteBiallelicCoalescentInterval(m, k, u, v, coalRate, coalIndex)
  else
    FiniteBiallelicCoalescentInterval(length, m, k, u, v, coalRate, coalIndex)

}

case class FiniteBiallelicCoalescentInterval(length: Double, m: Int, k: Int, u: Double, v: Double, coalRate: Double, coalIndex: Int) extends BiallelicCoalescentInterval {

  def updatedCoalRate(coalRate: Double): FiniteBiallelicCoalescentInterval = copy(coalRate = coalRate)

}

case class InfiniteBiallelicCoalescentInterval(m: Int, k: Int, u: Double, v: Double, coalRate: Double, coalIndex: Int) extends BiallelicCoalescentInterval {

  lazy val pi = {
    import spire.std.array._
    import spire.std.double._
    import spire.syntax.vectorSpace._
    val pihat = QMath.findOrthogonalVector(m, u, v, coalRate)
    val z = pihat(1) + pihat(2)
    pihat :/ z
  }

  def updatedCoalRate(coalRate: Double): InfiniteBiallelicCoalescentInterval = copy(coalRate = coalRate)

}
