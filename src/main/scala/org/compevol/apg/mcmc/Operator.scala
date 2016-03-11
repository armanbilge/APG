package org.compevol.apg.mcmc

import monocle.Lens

import scala.util.Random

abstract class Operator[T](implicit val random: Random) extends (T => T) {

  def apply(t: T): T

  def logHastingsRatio(x: T, y: T): Double

}

final class StateOperator[S, T](val lens: Lens[S, T], val op: Operator[T]) extends Operator[S]()(op.random) {

  override def apply(s: S): S = lens.modify(op)(s)

  override def logHastingsRatio(x: S, y: S): Double = op.logHastingsRatio(lens.get(x), lens.get(y))

}
