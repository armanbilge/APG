package org.compevol.apg

import no.uib.cipr.matrix.DenseVector

class LAVector(val m: Int, val f: Option[(Int, Int) => Double])(val index: LineageAlleleIndices = new LineageAlleleIndices(m)) extends DenseVector(index.size) {

  def apply(n: Int, r: Int): Double = get(index(n, r))

  def update(n: Int, r: Int, x: Double) = set(index(n, r), x)

}
