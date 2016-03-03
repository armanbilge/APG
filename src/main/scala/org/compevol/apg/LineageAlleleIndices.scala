package org.compevol.apg

final class LineageAlleleIndices(val m: Int) {

  @inline
  val size = m * (m + 3) / 2

  @inline
  def apply(n: Int, r: Int) = {
    require(1 <= n && n <= m)
    require(0 <= r && r <= n)
    (n - 1) * (n + 2) / 2 + r
  }

}
