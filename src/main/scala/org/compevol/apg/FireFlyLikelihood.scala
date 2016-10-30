package org.compevol.apg

class FireFlyLikelihood[P, D](params: P, data: IndexedSeq[D], L: (P, D) => Double, B: (P, D) => Double) extends (Set[Int] => Double) {

  class Cached(datum: D, f: (P, D) => Double) {
    lazy val value = f(params, datum)
  }

  val cachedLikelihoods = data.map(new Cached(_, L)).par
  val bounds = data.map(new Cached(_, B))

  override def apply(lit: Set[Int]): Double =
    cachedLikelihoods.zipWithIndex.map(Function.tupled((cl, i) => if (lit(i)) cl.value - bounds(i).value else bounds(i).value)).map(math.log).sum

}
