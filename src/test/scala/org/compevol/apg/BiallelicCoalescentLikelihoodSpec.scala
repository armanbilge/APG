package org.compevol.apg

import scala.collection.LinearSeq

class BiallelicCoalescentLikelihoodSpec extends UnitSpec {

  "BiallelicCoalescentLikelihood" when {

    "pi = (0.5, 0.5)" when {

      val like = new BiallelicSiteProbability(0.5)

      "intervals = [(length = Inf, m = 2, k = 2, u = 1, v = 1, 1/Ne = 1)]" when {

        val intervals = LinearSeq(InfiniteBiallelicCoalescentInterval(2, 2, 1, 1, 1))

        "samples = [[1, 0, 0]]" should {
          val samples = LinearSeq(IndexedSeq[Double](1, 0, 0))
          "be 0.3" in {
            assert(0.3 === like(intervals, samples))
          }
        }

        "samples = [[0, 1, 0]]" should {
          val sample = LinearSeq(IndexedSeq[Double](0, 1, 0))
          "be 0.4" in {
            assert(0.4 === like(intervals, sample))
          }
        }

        "samples = [[0, 0, 1]]" should {
          val sample = LinearSeq(IndexedSeq[Double](0, 0, 1))
          "be 0.3" in {
            assert(0.3 === like(intervals, sample))
          }
        }

      }

      "intervals = [(length = 1, m = 1, k = 1, u = 1, v = 1, 1/Ne = 1), (length = Inf, m = 2, k = 1, u = 1, v = 1, 1/Ne = 1)]" when {

        val intervals = LinearSeq(FiniteBiallelicCoalescentInterval(1, 1, 1, 1, 1, 1), InfiniteBiallelicCoalescentInterval(2, 1, 1, 1, 1))

        "samples = [[1, 0], [1, 0]]" should {
          val samples = LinearSeq(IndexedSeq[Double](1, 0), IndexedSeq[Double](1, 0))
          "be 0.25676676416183064" in {
            assert(0.25676676416183064 === like(intervals, samples))
          }
        }

        "samples = [[0, 1], [0, 1]]" should {
          val samples = LinearSeq(IndexedSeq[Double](0, 1), IndexedSeq[Double](0, 1))
          "be 0.25676676416183064" in {
            assert(0.25676676416183064 === like(intervals, samples))
          }
        }

        "samples = [[1, 0], [0, 1]]" should {
          val samples = LinearSeq(IndexedSeq[Double](1, 0), IndexedSeq[Double](0, 1))
          "be 0.24908421805557" in {
            assert(0.24323323583816936 === like(intervals, samples))
          }
        }

        "samples = [[0, 1], [1, 0]]" should {
          val samples = LinearSeq(IndexedSeq[Double](0, 1), IndexedSeq[Double](1, 0))
          "be 0.24908421805557" in {
            assert(0.24323323583816936 === like(intervals, samples))
          }
        }

      }

    }
  }

}
