package org.compevol.apg

import scala.collection.LinearSeq

class BiallelicCoalescentLikelihoodSpec extends UnitSpec {

  "BiallelicCoalescentLikelihood" when {

    "mu = 1.0, pi = (0.5, 0.5), intervals = [(length = Inf, Ne = 1.0)]" when {
      val like = new BiallelicCoalescentLikelihood(1.0, 0.5, LinearSeq(CoalescentInterval(Double.PositiveInfinity, 1.0)))
      "samples = [(t = 0, partial = [1, 0, 0])]" should {
        val sample = LinearSeq(TimePoint(0, IndexedSeq(1, 0, 0)))
        "be log(0.3)" in {
          assert(math.log(0.3) === like(sample))
        }
      }
      "samples = [(t = 0, partial = [0, 1, 0])]" should {
        val sample = LinearSeq(TimePoint(0, IndexedSeq(0, 1, 0)))
        "be log(0.4)" in {
          assert(math.log(0.4) === like(sample))
        }
      }
      "samples = [(t = 0, partial = [0, 0, 1])]" should {
        val sample = LinearSeq(TimePoint(0, IndexedSeq(0, 0, 1)))
        "be log(0.3)" in {
          assert(math.log(0.3) === like(sample))
        }
      }
      "samples = [(t = 0, partial = [1, 0]), (t = 1, partial = [1, 0])]" should {
        val sample = LinearSeq(TimePoint(0, IndexedSeq(1, 0)), TimePoint(1, IndexedSeq(1, 0)))
        "be log(0.25676676416183064)" in {
          assert(math.log(0.25676676416183064) === like(sample))
        }
      }
      "samples = [(t = 0, partial = [0, 1]), (t = 1, partial = [0, 1])]" should {
        val sample = LinearSeq(TimePoint(0, IndexedSeq(0, 1)), TimePoint(1, IndexedSeq(0, 1)))
        "be log(0.25676676416183064)" in {
          assert(math.log(0.25676676416183064) === like(sample))
        }
      }
      "samples = [(t = 0, partial = [1, 0]), (t = 1, partial = [0, 1])]" should {
        val sample = LinearSeq(TimePoint(0, IndexedSeq(1, 0)), TimePoint(1, IndexedSeq(0, 1)))
        "be log(0.24908421805557)" in {
          assert(math.log(0.24323323583816936) === like(sample))
        }
      }
      "samples = [(t = 0, partial = [0, 1]), (t = 1, partial = [1, 0])]" should {
        val sample = LinearSeq(TimePoint(0, IndexedSeq(0, 1)), TimePoint(1, IndexedSeq(1, 0)))
        "be log(0.24908421805557)" in {
          assert(math.log(0.24323323583816936) === like(sample))
        }
      }
    }
  }

}
