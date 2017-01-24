package apg

import apg.BiallelicSiteLikelihood.{Base, Nested}

class BiallelicCoalescentLikelihoodSpec extends UnitSpec {

  "BiallelicCoalescentLikelihood" when {

    "pi = (0.5, 0.5)" when {

      "intervals = [(length = Inf, m = 2, k = 2, u = 1, v = 1, 1/Ne = 1)]" when {

        "samples = [[1, 0, 0]]" should {
          "be 0.3" in {
            assert(0.3 === new BiallelicSiteLikelihood(0.5, new Base(BiallelicCoalescentInterval(length = Double.PositiveInfinity, m = 2, k = 2, u = 1, v = 1, coalRate = 1, coalIndex = 0), IndexedSeq(1.0, 0.0, 0.0).apply _)).evaluate)
          }
        }

        "samples = [[0, 1, 0]]" should {
          "be 0.4" in {
            assert(0.4 === new BiallelicSiteLikelihood(0.5, new Base(BiallelicCoalescentInterval(length = Double.PositiveInfinity, m = 2, k = 2, u = 1, v = 1, coalRate = 1, coalIndex = 0), IndexedSeq(0.0, 1.0, 0.0).apply _)).evaluate)
          }
        }

        "samples = [[0, 0, 1]]" should {
          "be 0.3" in {
            assert(0.3 === new BiallelicSiteLikelihood(0.5, new Base(BiallelicCoalescentInterval(length = Double.PositiveInfinity, m = 2, k = 2, u = 1, v = 1, coalRate = 1, coalIndex = 0), IndexedSeq(0.0, 0.0, 1.0).apply _)).evaluate)
          }
        }

      }

      "intervals = [(length = 1, m = 1, k = 1, u = 1, v = 1, 1/Ne = 1), (length = Inf, m = 2, k = 1, u = 1, v = 1, 1/Ne = 1)]" when {

        "samples = [[1, 0], [1, 0]]" should {
          "be 0.25676676416183064" in {
            assert(0.25676676416183064 === new BiallelicSiteLikelihood(0.5, new Nested(BiallelicCoalescentInterval(length = Double.PositiveInfinity, m = 2, k = 1, u = 1, v = 1, coalRate = 1, coalIndex = 0), IndexedSeq(1.0, 0.0).apply, new Base(BiallelicCoalescentInterval(length = 1, m = 1, k = 1, u = 1, v = 1, coalRate = 1, coalIndex = 0), IndexedSeq(1.0, 0.0).apply _))).evaluate)
          }
        }

        "samples = [[0, 1], [0, 1]]" should {
          "be 0.25676676416183064" in {
            assert(0.25676676416183064 === new BiallelicSiteLikelihood(0.5, new Nested(BiallelicCoalescentInterval(length = Double.PositiveInfinity, m = 2, k = 1, u = 1, v = 1, coalRate = 1, coalIndex = 0), IndexedSeq(0.0, 1.0).apply, new Base(BiallelicCoalescentInterval(length = 1, m = 1, k = 1, u = 1, v = 1, coalRate = 1, coalIndex = 0), IndexedSeq(0.0, 1.0).apply _))).evaluate)
          }
        }

        "samples = [[1, 0], [0, 1]]" should {
          "be 0.24908421805557" in {
            assert(0.24323323583816936 === new BiallelicSiteLikelihood(0.5, new Nested(BiallelicCoalescentInterval(length = Double.PositiveInfinity, m = 2, k = 1, u = 1, v = 1, coalRate = 1, coalIndex = 0), IndexedSeq(1.0, 0.0).apply, new Base(BiallelicCoalescentInterval(length = 1, m = 1, k = 1, u = 1, v = 1, coalRate = 1, coalIndex = 0), IndexedSeq(0.0, 1.0).apply _))).evaluate)
          }
        }

        "samples = [[0, 1], [1, 0]]" should {
          "be 0.24908421805557" in {
            assert(0.24323323583816936 === new BiallelicSiteLikelihood(0.5, new Nested(BiallelicCoalescentInterval(length = Double.PositiveInfinity, m = 2, k = 1, u = 1, v = 1, coalRate = 1, coalIndex = 0), IndexedSeq(0.0, 1.0).apply, new Base(BiallelicCoalescentInterval(length = 1, m = 1, k = 1, u = 1, v = 1, coalRate = 1, coalIndex = 0), IndexedSeq(1.0, 0.0).apply _))).evaluate)
          }
        }

      }

    }
  }

}
