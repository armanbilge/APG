package apg

class UpperTriangularMatrix(val n: Int) {

  val values = new Array[Double](n * (n + 1) / 2)

  def index(i: Int, j: Int): Int = {
    require(j >= i)
    val nmi = n - i
    (n * (n + 1) - nmi * (nmi + 1)) / 2 + (j - i)
  }

  def apply(i: Int, j: Int): Double = values(index(i, j))

  def update(i: Int, j: Int, x: Double): Unit = values(index(i, j)) = x

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
    val P = new UpperTriangularMatrix(n)
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
