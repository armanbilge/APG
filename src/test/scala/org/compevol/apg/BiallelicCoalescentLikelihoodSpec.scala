package org.compevol.apg

import scala.collection.LinearSeq

class BiallelicCoalescentLikelihoodSpec extends UnitSpec {

  "BiallelicCoalescentLikelihood" when {

    "mu = 2.0, pi = (0.5, 0.5), intervals = [(length = Inf, Ne = 1.0)]" when {
      val like = new BiallelicCoalescentLikelihood(2.0, 0.5, LinearSeq(CoalescentInterval(Double.PositiveInfinity, 1.0)))
      "samples = [(t = 0, p_red = [0.0, 0.0])]" should {
        val sample = LinearSeq(BiallelicSample(0, Seq(0.0, 0.0)))
        "be 0.3" in {
          assert(0.3 === like(sample))
        }
      }
      "samples = [(t = 0, p_red = [1.0, 0.0])]" should {
        val sample = LinearSeq(BiallelicSample(0, Seq(1.0, 0.0)))
        "be 0.4" in {
          assert(0.4 === like(sample))
        }
      }
      "samples = [(t = 0, p_red = [0.0, 1.0])]" should {
        val sample = LinearSeq(BiallelicSample(0, Seq(0.0, 1.0)))
        "be 0.4" in {
          assert(0.4 === like(sample))
        }
      }
      "samples = [(t = 0, p_red = [1.0, 1.0])]" should {
        val sample = LinearSeq(BiallelicSample(0, Seq(1.0, 1.0)))
        "be 0.3" in {
          assert(0.3 === like(sample))
        }
      }
    }
  }

}
