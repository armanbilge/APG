package org.compevol.apg

import no.uib.cipr.matrix.DenseMatrix

class Q(val m: Int, val u: Double, val v: Double, val Ne: Double)(index: LineageAlleleIndices = new LineageAlleleIndices(m)) extends DenseMatrix(index.size, index.size) {

  for {
    n <- 1 to m
    r <- 0 to n
  } {
    val i = index(n, r)
    if (r > 0) set(i, index(n, r - 1), (n - r + 1) * v)
    if (r < n) set(i, index(n, r + 1), (r + 1) * u)
    if (n > 1) set(i, index(n - 1, r), (n - 1 - r) * n / Ne)
    if (r > 0 && n > 1) set(i, index(n - 1, r - 1), (r - 1) * n / Ne)
    set(i, i, - n * (n - 1) / Ne - (n - r) * v - r * u)
  }

}
