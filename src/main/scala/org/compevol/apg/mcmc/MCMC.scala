package org.compevol.apg.mcmc

import java.util

import org.apache.commons.math3.distribution.EnumeratedDistribution
import org.apache.commons.math3.random.RandomGenerator
import org.apache.commons.math3.util.Pair

case class State[S](state: S, logP: Double)

class MCMC[S](val posterior: S => Double, val operators: Map[Operator[S], Double])(implicit val random: RandomGenerator) {

  val operatorDistribution = new EnumeratedDistribution[Operator[S]](random, util.Arrays.asList(operators.map(Function.tupled((op, w) => new Pair(op, Double.box(w)))).toSeq: _*))

  def chain(start: S): Iterator[(Int, Double, S)] = Iterator.iterate(State(start, posterior(start))) { s =>
    val op = operatorDistribution.sample()
    val sp = op(s.state)
    val spLogP = posterior(sp)
    val alpha = 0.0 min (spLogP - s.logP + op.logHastingsRatio(s.state, sp))
    if (Math.log(random.nextDouble()) < alpha)
      State(sp, spLogP)
    else
      s
  }.zipWithIndex.map(s => (s._2, s._1.logP, s._1.state))

}
