package org.compevol.apg

import scala.collection.LinearSeq

object Main extends App {

  val like = new BiallelicCoalescentLikelihood(1.0, (0.5, 0.5), LinearSeq(CoalescentInterval(Double.MaxValue, 1.0)))
  val sample = LinearSeq(BiallelicSample(0, Seq(0.0, 0.0)))
  println(like(sample))

}
