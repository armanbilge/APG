package apg

import mcmc.Probability
import monocle.Lens
import shapeless.tag
import shapeless.tag.@@

class DatumLikelihood[B, P <: Probability[Double], L1 <: Probability[Double], L2 <: Probability[Double]](val lit: Int @@ B, val probability: P, val lower1: L1, val lower2: L2) extends Probability[Double] {

  override def evaluate: Double = math.log(lit.toInt match {
    case 0 => lower2.evaluate
    case 1 => math.max(lower1.evaluate - lower2.evaluate, 0)
    case 2 => math.max(probability.evaluate - lower1.evaluate, 0)
  })

  def light(b: Int @@ B): DatumLikelihood[B, P, L1, L2] = if (lit == b) this else new DatumLikelihood(b, probability, lower1, lower2)
  def on: DatumLikelihood[B, P, L1, L2] = light(tag[B](2))
  def dim: DatumLikelihood[B, P, L1, L2] = light(tag[B](1))
  def off: DatumLikelihood[B, P, L1, L2] = light(tag[B](0))

  def entropy: Double = {
    val p0 = lower2.evaluate / probability.evaluate
    val p1 = (lower1.evaluate - lower2.evaluate) / probability.evaluate
    val p2 = (probability.evaluate - lower1.evaluate) / probability.evaluate
    var e = 0.0
    if (p0 > 0) e -= p0 * math.log(p0)
    if (p1 > 0) e -= p1 * math.log(p1)
    if (p2 > 0) e -= p2 * math.log(p2)
    e
  }

}

object DatumLikelihood {

  implicit def lit[B, P <: Probability[Double], L1 <: Probability[Double], L2 <: Probability[Double]]: Lens[DatumLikelihood[B, P, L1, L2], Int @@ B] =
    Lens[DatumLikelihood[B, P, L1, L2], Int @@ B](_.lit)(b => dl => new DatumLikelihood(b, dl.probability, dl.lower1, dl.lower2))

}
