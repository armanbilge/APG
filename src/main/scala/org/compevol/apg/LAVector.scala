package org.compevol.apg

import no.uib.cipr.matrix.DenseVector

class LAVector(val m: Int)(val index: LineageAlleleIndices = new LineageAlleleIndices(m))(val f: (Int, Int) => Double = (_, _) => 0) extends DenseVector(index.size) {

  index.foreach(f)

  def apply(n: Int, r: Int): Double = get(index(n, r))

  def update(n: Int, r: Int, x: Double) = set(index(n, r), x)

}
