package apg

final class ArrayUpperTriangularMatrix(override val n: Int) extends UpperTriangularMatrix {

  val values = new Array[Double](n * (n + 1) / 2)

  override def apply(i: Int, j: Int): Double = values(index(i, j))

  def update(i: Int, j: Int, x: Double): Unit = values(index(i, j)) = x

  @inline def index(i: Int, j: Int): Int = {
    val nmi = n - i
    (n * (n + 1) - nmi * (nmi + 1)) / 2 + (j - i)
  }

}
