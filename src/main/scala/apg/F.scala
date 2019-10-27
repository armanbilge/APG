package apg

class F(val N: Int, val R: Int, val f: Array[FP]) {

  def index(n: Int, r: Int): Int = math.min(n,R)*(math.min(n,R)+1)/2-1 + math.max(n-R,0)*(R+1) + r

  def apply(n: Int, r: Int): FP = f(index(n, r))

  def update(n: Int, r: Int, x: FP): Unit = f(index(n, r)) = x

}

object F {

  def apply(N: Int, R: Int): F = new F(N, R, new Array((R+1)*(R+2)/2-1 + (N-R)*(R+1)))

}
