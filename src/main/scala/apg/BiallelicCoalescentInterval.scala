package apg

sealed trait BiallelicCoalescentInterval {
  val m: Int
  val k: Int
  val coalIndex: Int
  val partial: Int => Double
  def updatedUV(u: Double, v: Double): BiallelicCoalescentInterval
  def updatedCoalRate(coalRate: Double): BiallelicCoalescentInterval
}

object BiallelicCoalescentInterval {

  def apply(length: Double, m: Int, k: Int, u: Double, v: Double, coalRate: Double, coalIndex: Int, partial: Int => Double) = if (length.isInfinity)
    InfiniteBiallelicCoalescentInterval(m, k, u, v, coalRate, coalIndex, partial)
  else
    FiniteBiallelicCoalescentInterval(length, m, k, u, v, coalRate, coalIndex, partial)

}

case class FiniteBiallelicCoalescentInterval(length: Double, m: Int, k: Int, u: Double, v: Double, coalRate: Double, coalIndex: Int, partial: Int => Double) extends BiallelicCoalescentInterval {

  def updatedUV(u: Double, v: Double): FiniteBiallelicCoalescentInterval = copy(u = u, v = v)
  def updatedCoalRate(coalRate: Double): FiniteBiallelicCoalescentInterval = copy(coalRate = coalRate)

}

case class InfiniteBiallelicCoalescentInterval(m: Int, k: Int, u: Double, v: Double, coalRate: Double, coalIndex: Int, partial: Int => Double) extends BiallelicCoalescentInterval {

  val pi = {
    import spire.std.array._
    import spire.std.double._
    import spire.syntax.vectorSpace._
    val pihat = new Q(m, u, v, coalRate).findOrthogonalVector(false)
    val z = pihat(1) + pihat(2)
    pihat :/ z
  }

  def updatedUV(u: Double, v: Double): InfiniteBiallelicCoalescentInterval = copy(u = u, v = v)
  def updatedCoalRate(coalRate: Double): InfiniteBiallelicCoalescentInterval = copy(coalRate = coalRate)

}
