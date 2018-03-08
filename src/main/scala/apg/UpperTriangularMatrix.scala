package apg

trait UpperTriangularMatrix {

  val n: Int

  def apply(i: Int, j: Int): Double

  def *(x: IndexedSeq[Double]): Array[Double] = {
    Array.tabulate(x.length) { i =>
      (i until x.length).view.map(j => this(i, j) * x(j)).sum
    }
  }

  def `^-1 *`(x: IndexedSeq[Double]): Array[Double] = {
    val y = new Array[Double](x.length)
    for (i <- y.indices.reverse) {
      y(i) = (x(i) - (i+1 until x.length).map(j => this(i, j) * y(j)).sum) / this(i, i)
    }
    y
  }

  val eigenValues: IndexedSeq[Double] = new IndexedSeq[Double] {
    override def length = n
    override def apply(i: Int) = UpperTriangularMatrix.this(i, i)
  }

  def eigenVectors: UpperTriangularMatrix = {
    val P = new ArrayUpperTriangularMatrix(n)
    for (i <- 0 until n) {
      P(i, i) = 1
      for (j <- (0 until i).reverse) {
        if (this(j, j) - this(i, i) != 0)
          P(j, i) = - (j+1 until (i+1)).map(k => this(j, k) * P(k, i)).sum / (this(j, j) - eigenValues(i))
      }
    }
    P
  }

}
