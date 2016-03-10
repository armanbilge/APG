package org.compevol.apg

final class LineageAlleleIndices(val m: Int) {

  require(m >= 1)

  @inline
  val size = m * (m + 3) / 2

  @inline
  def apply(n: Int, r: Int) = {
    require(1 <= n && n <= m)
    require(0 <= r && r <= n)
    (n - 1) * (n + 2) / 2 + r
  }

  @inline
  def foreach[U](f: (Int, Int) => U) = for {
    n <- 1 to m
    r <- 0 to n
  } f(n, r)

}
