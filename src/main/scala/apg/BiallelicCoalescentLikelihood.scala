package apg

import apg.BiallelicCoalescentLikelihood.{ExactIntegrator, InfiniteIntegrator, Lower, createIntervals}
import mcmc.{Categorical, Probability}
import monocle.Lens
import monocle.function.At
import shapeless.tag
import shapeless.tag.@@
import spire.random.Generator

import scala.annotation.tailrec
import scala.collection.{LinearSeq, mutable}
import scala.language.higherKinds

class BiallelicCoalescentLikelihood[D[X], I[X], B, M, Θ](val lights: D[DatumLikelihood[B, BiallelicSiteLikelihood, BiallelicSiteLikelihood, Lower]],
                                                         val exactIntegrators: Seq[ExactIntegrator],
                                                         val infiniteIntegrator: InfiniteIntegrator,
                                                         val constantData: LinearSeq[IndexedSeq[Double]],
                                                         val constantSiteLike: BiallelicSiteLikelihood,
                                                         val datum: LinearSeq[TimePoint],
                                                         val mu: Double @@ M,
                                                         val coalIntervals: LinearSeq[CoalescentInterval[Θ]],
                                                         val age: Long
                                                        )(implicit val distributed: Distributed[D, I]) extends Probability[Double] {

  import distributed._

  if (age % 50 == 0) lights.checkpoint()

  lazy val evaluate: Double = lights.map(_.evaluate).sum

  lazy val fractionOn: Double = lights.map(l => if (l.lit == 2) 1.0 else 0.0).sum / lights.size
  lazy val fractionDim: Double = lights.map(l => if (l.lit == 1) 1.0 else 0.0).sum / lights.size

  def updated(mu: Double @@ M = mu): BiallelicCoalescentLikelihood[D, I, B, M, Θ] = {

    val infiniteInterval :: finiteIntervals = createIntervals(mu, 0.5, coalIntervals, datum)
    val infiniteIntegrator = new InfiniteIntegrator(infiniteInterval)
    val intervals = (infiniteInterval :: finiteIntervals).reverse
    val exactIntegrators = finiteIntervals.reverse.map(new ExactIntegrator(_))
    val eitherExactIntegrators = exactIntegrators.map(Left(_)) ::: Right(infiniteIntegrator) :: Nil
    val constantSiteLike = BiallelicSiteLikelihood(intervals, constantData, eitherExactIntegrators)
    val updatedExactIntegrators = eitherExactIntegrators.zipWithIndex.map(_.swap).toMap
    val updatedApproximateIntegrators = eitherExactIntegrators.zipWithIndex.map(_.swap).toMap
    val lights = this.lights.map { light =>
      new DatumLikelihood(light.lit, light.probability.updateIntegrators(updatedExactIntegrators), light.lower1.updateIntegrators(updatedApproximateIntegrators), new Lower(light.lower2.data, constantSiteLike.evaluate))
    }.persist()
    new BiallelicCoalescentLikelihood(lights, exactIntegrators, infiniteIntegrator, constantData, constantSiteLike, datum, mu, coalIntervals, age + 1)

  }

}

object BiallelicCoalescentLikelihood {

  def apply[D[X], I[X], B, M, Θ](mu: Double @@ M, coalIntervals: LinearSeq[CoalescentInterval[Θ]], data: D[LinearSeq[TimePoint]])(implicit distributed: Distributed[D, I], rng: Generator): BiallelicCoalescentLikelihood[D, I, B, M, Θ] = {

    apply[D, I, B, M, Θ]((i: Int, like: BiallelicSiteLikelihood, lower1: BiallelicSiteLikelihood, lower2: Lower) => {
      val on = new DatumLikelihood[B, BiallelicSiteLikelihood, BiallelicSiteLikelihood, Lower](tag[B](2), like, lower1, lower2)
      val dim = on.dim
      val off = on.off
      import spire.std.double._
      val P = math.exp(on.evaluate) + math.exp(dim.evaluate) + math.exp(off.evaluate)
      val p = IndexedSeq(math.exp(on.evaluate)/P, math.exp(dim.evaluate)/P, math.exp(off.evaluate)/P)
      rng.next(Categorical(Map(on -> math.exp(on.evaluate), dim -> math.exp(dim.evaluate), off -> math.exp(off.evaluate))))
    }, mu, coalIntervals, data)

  }

  def apply[D[X], I[X], B, M, Θ](f: (Int, BiallelicSiteLikelihood, BiallelicSiteLikelihood, Lower) => DatumLikelihood[B, BiallelicSiteLikelihood, BiallelicSiteLikelihood, Lower], mu: Double @@ M, coalIntervals: LinearSeq[CoalescentInterval[Θ]], data: D[LinearSeq[TimePoint]])(implicit distributed: Distributed[D, I]): BiallelicCoalescentLikelihood[D, I, B, M, Θ] = {

    import distributed._

    val datum = data.head
    val constantData = List.fill(datum.size)(IndexedSeq(1.0))
    val infiniteInterval :: finiteIntervals = createIntervals(mu, 0.5, coalIntervals, datum)
    val infiniteIntegrator = new InfiniteIntegrator(infiniteInterval)
    val intervals = (infiniteInterval :: finiteIntervals).reverse
    val exactIntegrators = finiteIntervals.reverse.map(new ExactIntegrator(_))
    val eitherExactIntegrators = exactIntegrators.map(Left(_)) ::: Right(infiniteIntegrator) :: Nil
    val constantSiteLike = BiallelicSiteLikelihood(intervals, constantData, eitherExactIntegrators)
    val lights = data.zipWithIndexMap { (sample, i) =>
      val partials = sample.map(_.redCountPartial)
      val n = partials.map(_.length - 1).sum
      def minimizeExpectation(partials: Seq[IndexedSeq[Double]]) = {
        val like = BiallelicSiteLikelihood(intervals, partials, eitherExactIntegrators)
        val lower2 = new Lower(partials.map(_.head).product, constantSiteLike.evaluate)
        if (lower2.evaluate / like.evaluate > 0.99) {
          (0.0, like, BiallelicSiteLikelihood(intervals, partials.map(p => p.take(p.zipWithIndex.maxBy(_._1)._2 + 1)), eitherExactIntegrators), lower2)
        } else {
          val take = partials.map(_ => 1).toArray
          var expectation = Double.MaxValue
          take.indices.foreach { i =>
            expectation = Double.MaxValue
            var continue = true
            while (continue && take(i) <= partials(i).length) {
              val newPartials = (partials, take).zipped.map(_.take(_))
              val lower1 = BiallelicSiteLikelihood(intervals, newPartials, eitherExactIntegrators)
              val on = new DatumLikelihood[B, BiallelicSiteLikelihood, BiallelicSiteLikelihood, Lower](tag[B](2), like, lower1, lower2)
              var onP = on.evaluate
              var dimP = on.dim.evaluate
              val maxP = math.max(onP, dimP)
              onP = math.exp(onP - maxP)
              dimP = math.exp(dimP - maxP)
              val Z = onP + dimP
              onP /= Z
              dimP /= Z
              def ops(n: Int, r: Int): Int = (r+1)*(r+2)/2-1 + (n-r)*(r+1)
              val e = onP * ops(n, n) + dimP * ops(n, take.sum - take.length)
              if (e < expectation)
                expectation = e
              else
                continue = false
              take(i) += 1
            }
            take(i) -= 1
          }
          val newPartials = (partials, take).zipped.map(_.take(_))
          val lower1 = BiallelicSiteLikelihood(intervals, newPartials, eitherExactIntegrators)
          (expectation, like, lower1, lower2)
        }
      }
      val x = minimizeExpectation(partials)
      val y = minimizeExpectation(partials.map(_.reverse))
      val z = if (x._1 < y._1) x else y
      f(i.toInt, z._2, z._3, z._4)
    }.persist()
    new BiallelicCoalescentLikelihood[D, I, B, M, Θ](lights, exactIntegrators, infiniteIntegrator, constantData, constantSiteLike, datum, mu, coalIntervals, 0)

  }

  class InfiniteIntegrator(val interval: BiallelicCoalescentInterval) extends (F => Double) {
    val pi = Q.findOrthogonalVector(interval.m, interval.u, interval.v, interval.coalRate)
    override def apply(f: F): Double = {
      var sum = 0.0
      import spire.syntax.cfor._
      var i = 0
      var j = 0
      cforRange(1 to f.R) { n =>
        cforRange(0 to n) { _ =>
          sum += pi(i) * f.f(j)
          i += 1
          j += 1
        }
      }
      cforRange2((f.R+1) to f.N, 0 to f.R) { (n, r) =>
        sum += pi(i) * f.f(j)
        i += 1
        j += 1
        if (f.R == r) {
          i += n - r
        }
      }
      sum
    }
  }

  class ExactIntegrator(val interval: BiallelicCoalescentInterval) extends (F => F) {
    override def apply(f: F): F = Q.expQTtx(12, 1, interval.u, interval.v, interval.coalRate, interval.length, f)
  }

  class Lower(val data: Double, val prior: Double) extends Probability[Double] with Serializable {
    val evaluate: Double = data * prior
  }

  def createIntervals[Θ](mu: Double, piRed: Double, coalIntervals: LinearSeq[CoalescentInterval[Θ]], samples: LinearSeq[TimePoint]): List[BiallelicCoalescentInterval] = {

    val piGreen = 1 - piRed
    val beta = 1 / (1 - piRed * piRed - piGreen * piGreen)

    val u = beta * mu * piRed
    val v = beta * mu * piGreen

    @tailrec
    def recurse(intervals: LinearSeq[CoalescentInterval[Θ]], samples: LinearSeq[TimePoint], nextCoal: Double, t: Double = 0, m: Int = 0, coalIndex: Int = 0, acc: List[BiallelicCoalescentInterval] = Nil): List[BiallelicCoalescentInterval] = if (t.isPosInfinity)
      acc
    else {

      val nextSample = samples.headOption.map(_.t).getOrElse(Double.PositiveInfinity)
      val interval :: intervalsTail = intervals

      val (intervalP, intervalsP, nextCoalP, coalIndexP) = if (t == nextCoal) {
        val nextInterval = intervalsTail.head
        val nextNextCoal = nextCoal + nextInterval.length
        val coalIndexP = coalIndex + 1
        (nextInterval, intervalsTail, nextNextCoal, coalIndexP)
      } else (interval, intervals, nextCoal, coalIndex)

      val (nextSampleP, samplesP, k) = if (t == nextSample) {
        val sample :: samplesTail = samples
        val nextNextSample = samplesTail.headOption.map(_.t).getOrElse(Double.PositiveInfinity)
        val k = sample.k
        (nextNextSample, samplesTail, k)
      } else (nextSample, samples, 0)

      val mp = m + k
      val nextEvent = nextCoalP min nextSampleP

      recurse(intervalsP, samplesP, nextCoalP, nextEvent, mp, coalIndexP, BiallelicCoalescentInterval(nextEvent - t, mp, k, u, v, 1 / intervalP.Ne, coalIndexP) :: acc)

    }

    recurse(coalIntervals, samples, coalIntervals.head.length)

  }

  implicit def mu[D[X], I[X], B, M, Θ]: Lens[BiallelicCoalescentLikelihood[D, I, B, M, Θ], Double @@ M] = Lens[BiallelicCoalescentLikelihood[D, I, B, M, Θ], Double @@ M](_.mu)(mu => _.updated(mu = mu))

  implicit def atNe[D[X], I[X], B, M, Θ]: At[BiallelicCoalescentLikelihood[D, I, B, M, Θ], Int, Double @@ Θ] = new At[BiallelicCoalescentLikelihood[D, I, B, M, Θ], Int, Double @@ Θ] {
    override def at(i: Int): Lens[BiallelicCoalescentLikelihood[D, I, B, M, Θ], @@[Double, Θ]] = {
      Lens[BiallelicCoalescentLikelihood[D, I, B, M, Θ], Double @@ Θ](_.coalIntervals(i).Ne)(Ne => { bcl =>
        implicit val distributed = bcl.distributed
        import distributed._
        val coalRate = 1 / Ne
        val coalIntervals = bcl.coalIntervals.updated(i, bcl.coalIntervals(i).copy(Ne = Ne))
        val updatedExactIntegrators = mutable.Map[Int, Either[F => F, F => Double]]()
        val exactIntegrators = bcl.exactIntegrators.zipWithIndex.map { case (integrator, j) =>
          if (integrator.interval.coalIndex == i) {
            val newIntegrator = new ExactIntegrator(integrator.interval.copy(coalRate = coalRate))
            updatedExactIntegrators += j -> Left(newIntegrator)
            newIntegrator
          } else
            integrator
        }
        val infiniteIntegrator = if (bcl.infiniteIntegrator.interval.coalIndex == i) {
          val newIntegrator = new InfiniteIntegrator(bcl.infiniteIntegrator.interval.copy(coalRate = coalRate))
          updatedExactIntegrators += exactIntegrators.size -> Right(newIntegrator)
          newIntegrator
        } else
          bcl.infiniteIntegrator
        val constantSiteLike = bcl.constantSiteLike.updateIntegrators(updatedExactIntegrators)
        val lights = bcl.lights.map { light =>
          new DatumLikelihood(light.lit, light.probability.updateIntegrators(updatedExactIntegrators), light.lower1.updateIntegrators(updatedExactIntegrators), new Lower(light.lower2.data, constantSiteLike.evaluate))
        }.persist()
        new BiallelicCoalescentLikelihood[D, I, B, M, Θ](lights, exactIntegrators, infiniteIntegrator, bcl.constantData, constantSiteLike, bcl.datum, bcl.mu, coalIntervals, bcl.age + 1)
      })
    }
  }

  implicit def lights[D[X], I[X], B, M, Θ]: Lens[BiallelicCoalescentLikelihood[D, I, B, M, Θ], D[DatumLikelihood[B, BiallelicSiteLikelihood, BiallelicSiteLikelihood, Lower]]] =
    Lens[BiallelicCoalescentLikelihood[D, I, B, M, Θ], D[DatumLikelihood[B, BiallelicSiteLikelihood, BiallelicSiteLikelihood, Lower]]](_.lights)(lights => bcl => new BiallelicCoalescentLikelihood(lights, bcl.exactIntegrators, bcl.infiniteIntegrator, bcl.constantData, bcl.constantSiteLike, bcl.datum, bcl.mu, bcl.coalIntervals, bcl.age + 1)(bcl.distributed))

}
