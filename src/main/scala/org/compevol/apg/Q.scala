package org.compevol.apg

import no.uib.cipr.matrix.DenseMatrix

class Q(val m: Int, val u: Double, val v: Double, val Ne: Double)(index: LineageAlleleIndices = new LineageAlleleIndices(m)) extends DenseMatrix(index.size, index.size) {

  index.foreach { (n, r) =>
    val i = index(n, r)
    if (r > 0) set(index(n, r - 1), i, (n - r + 1) * v)
    if (r < n) set(index(n, r + 1), i, (r + 1) * u)
    if (n > 1 && r < n) set(index(n - 1, r), i, (n - 1 - r) * n / Ne)
    if (r > 0 && n > 1) set(index(n - 1, r - 1), i, (r - 1) * n / Ne)
    set(i, i, - n * (n - 1) / Ne - (n - r) * v - r * u)
  }

}
