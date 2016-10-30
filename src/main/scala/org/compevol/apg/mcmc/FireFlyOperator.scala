package org.compevol.apg.mcmc

import org.apache.commons.math3.random.RandomGenerator

class FireFlyOperator(val n: Int)(implicit random: RandomGenerator) extends Operator[Set[Int]] {

  override def apply(t: Set[Int]): Set[Int] = {
    val i = random.nextInt(n)
    if (!t(i)) t + i else t - i
  }

  override def logHastingsRatio(x: Set[Int], y: Set[Int]): Double = 1

}
