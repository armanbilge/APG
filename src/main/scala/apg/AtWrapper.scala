package apg

import mcmc.Probability
import monocle.Lens
import monocle.function.At

class AtWrapper[P <: Probability[Double]](val p: P, val i: Int) extends Probability[Double] {
  override def evaluate: Double = p.evaluate
}

object AtWrapper {

  implicit def atP[P <: Probability[Double], T](implicit p_t: Lens[P, T]): At[AtWrapper[P], Int, T] = new At[AtWrapper[P], Int, T] {
    override def at(i: Int): Lens[AtWrapper[P], T] =
      Lens[AtWrapper[P], T](w => if (i == w.i) p_t.get(w.p) else null.asInstanceOf[T])(t => w => if (i == w.i) new AtWrapper(p_t.set(t)(w.p), i) else w)

  }

}
