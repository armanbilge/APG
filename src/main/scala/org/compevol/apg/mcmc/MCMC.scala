package org.compevol.apg.mcmc

import scala.util.Random

case class State[S](state: S, logP: Double)

class MCMC[S](val posterior: S => Double, val operators: Map[Operator[S], Double])(implicit val random: Random) {

  def chain(start: S): Iterator[S] = Iterator.iterate(State(start, posterior(start))) { s =>
    val op = operators.keys.last
    val sp = op(s.state)
    val spLogP = posterior(sp)
    val alpha = 0.0 min (spLogP - s.logP + op.logHastingsRatio(s.state, sp))
    if (Math.log(random.nextDouble()) < alpha)
      State(sp, spLogP)
    else
      s
  }.map(_.state)

}
