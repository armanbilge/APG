package apg

import apg.BiallelicCoalescentLikelihood.Lower
import mcmc.Probability
import monocle.Lens
import monocle.function.At
import shapeless.tag
import shapeless.tag.@@
import spire.random.Generator

import scala.annotation.tailrec
import scala.collection.LinearSeq
import scala.language.higherKinds

class BiallelicCoalescentLikelihood[D[X], I[X], B, M, Π, Θ](val lights: D[DatumLikelihood[B, BiallelicSiteLikelihood, Lower]],
                                                            val greenBound: BiallelicSiteLikelihood,
                                                            val redBound: BiallelicSiteLikelihood,
                                                            val mu: Double @@ M,
                                                            val piRed: Double @@ Π,
                                                            val infiniteInterval: I[InfiniteBiallelicCoalescentInterval],
                                                            val coalIntervals: LinearSeq[CoalescentInterval[Θ]],
                                                            val datum: LinearSeq[TimePoint],
                                                            val greenData: LinearSeq[Int => Double],
                                                            val redData: LinearSeq[Int => Double],
                                                            val age: Long
                                                           )(implicit val distributed: Distributed[D, I]) extends Probability[Double] {

  import distributed._

  if (age % 50 == 0) lights.checkpoint()

  lazy val evaluate: Double = lights.map(_.evaluate).sum

  lazy val fractionLit: Double = lights.map(l => if (l.lit) 1.0 else 0.0).sum / lights.size

  def updated(mu: Double @@ M = mu, piRed: Double @@ Π = piRed): BiallelicCoalescentLikelihood[D, I, B, M, Π, Θ] = {

    val (intervals, infiniteInterval) = BiallelicCoalescentLikelihood.createIntervals(mu, piRed, coalIntervals, datum)
    val broadcastedInfiniteInterval = distributed.broadcast(infiniteInterval)
    val greenBound = BiallelicSiteLikelihood(piRed, distributed.retrieve(broadcastedInfiniteInterval), intervals, greenData)
    val redBound = BiallelicSiteLikelihood(piRed, distributed.retrieve(broadcastedInfiniteInterval), intervals, redData)
    val greenP = greenBound.evaluate
    val redP = redBound.evaluate
    val myDistributed = distributed
    val lights = this.lights.map { light =>
      val bound = if (light.lower.red)
        new Lower(redP, light.lower.q, true)
      else
        new Lower(greenP, light.lower.q, false)
      new DatumLikelihood(light.lit, BiallelicSiteLikelihood(piRed, myDistributed.retrieve(broadcastedInfiniteInterval), intervals, light.probability.partials), bound)
    }.persist()
    new BiallelicCoalescentLikelihood(lights, greenBound, redBound, mu, piRed, broadcastedInfiniteInterval, coalIntervals, datum, greenData, redData, age + 1)

  }

}

object BiallelicCoalescentLikelihood {

  def apply[D[X], I[X], B, M, Π, Θ](mu: Double @@ M, piRed: Double @@ Π, coalIntervals: LinearSeq[CoalescentInterval[Θ]], data: D[LinearSeq[TimePoint]])(implicit distributed: Distributed[D, I], rng: Generator): BiallelicCoalescentLikelihood[D, I, B, M, Π, Θ] = {

    apply[D, I, B, M, Π, Θ]((i: Int, like: BiallelicSiteLikelihood, bound: Lower) => {
      val dl = new DatumLikelihood[B, BiallelicSiteLikelihood, Lower](tag[B](false), like, bound)
      val flipped = new DatumLikelihood[B, BiallelicSiteLikelihood, Lower](tag[B](true), like, bound)
      if (flipped.evaluate.isInfinite && dl.evaluate.isInfinite)
        throw new RuntimeException(s"sLikelihoods are infinite at site $i")
      if (flipped.evaluate.isNaN || dl.evaluate.isNaN)
        throw new RuntimeException(s"Encountered NaN at site $i: ${flipped.evaluate} or ${dl.evaluate}")
      val oddsRatio = math.exp(flipped.evaluate - dl.evaluate)
      if (flipped.evaluate.isInfinite)
        dl
      else if (dl.evaluate.isInfinite)
        flipped
      else if (rng.nextDouble() < oddsRatio / (oddsRatio + 1))
        flipped
      else
        dl
    }, mu, piRed, coalIntervals, data)

  }

  def apply[D[X], I[X], B, M, Π, Θ](lit: IndexedSeq[Boolean @@ B], mu: Double @@ M, piRed: Double @@ Π, coalIntervals: LinearSeq[CoalescentInterval[Θ]], data: D[LinearSeq[TimePoint]])(implicit distributed: Distributed[D, I]): BiallelicCoalescentLikelihood[D, I, B, M, Π, Θ] = {

    apply[D, I, B, M, Π, Θ]((i: Int, like: BiallelicSiteLikelihood, bound: Lower) => new DatumLikelihood[B, BiallelicSiteLikelihood, Lower](lit(i), like, bound), mu, piRed, coalIntervals, data)

  }

  def apply[D[X], I[X], B, M, Π, Θ](f: (Int, BiallelicSiteLikelihood, Lower) => DatumLikelihood[B, BiallelicSiteLikelihood, Lower], mu: Double @@ M, piRed: Double @@ Π, coalIntervals: LinearSeq[CoalescentInterval[Θ]], data: D[LinearSeq[TimePoint]])(implicit distributed: Distributed[D, I]): BiallelicCoalescentLikelihood[D, I, B, M, Π, Θ] = {

    import distributed._

    val head = data.head
    val greenData = List.fill(head.size)((i: Int) => if (i == 0) 1.0 else 0.0)
    val redData = head.map(_.redCountPartial.length - 1).map(k => (i: Int) => if (i == k) 1.0 else 0.0)
    val (intervals, infiniteInterval) = createIntervals(mu, piRed, coalIntervals, head)
    val broadcastedInfiniteInterval = distributed.broadcast(infiniteInterval)
    val greenBound = BiallelicSiteLikelihood(piRed, distributed.retrieve(broadcastedInfiniteInterval), intervals, greenData)
    val redBound = BiallelicSiteLikelihood(piRed, distributed.retrieve(broadcastedInfiniteInterval), intervals, redData)
    val greenP = greenBound.evaluate
    val redP = redBound.evaluate
    val lights = data.zipWithIndexMap { (sample, i) =>
      val partials = sample.map(_.redCountPartial)
      val like = BiallelicSiteLikelihood(piRed, distributed.retrieve(broadcastedInfiniteInterval), intervals, partials.toList)
      val greenScaler = partials.map(_.head).product
      val redScaler = partials.map(_.last).product
      val bound = if (greenScaler > redScaler)
        new Lower(greenP, greenScaler, false)
      else
        new Lower(redP, redScaler, true)
      f(i.toInt, like, bound)
    }.persist()
    new BiallelicCoalescentLikelihood[D, I, B, M, Π, Θ](lights, greenBound, redBound, mu, piRed, broadcastedInfiniteInterval, coalIntervals, head, greenData, redData, 0)

  }

  class Lower(val p: Double, val q: Double, val red: Boolean) extends Probability[Double] with Serializable {
    val evaluate: Double = p * q
  }

  def createIntervals[Θ](mu: Double, piRed: Double, coalIntervals: LinearSeq[CoalescentInterval[Θ]], samples: LinearSeq[TimePoint]): (List[BiallelicCoalescentInterval], InfiniteBiallelicCoalescentInterval) = {

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

    val intervals = recurse(coalIntervals, samples, coalIntervals.head.length)
    (intervals.reverse, intervals.head.asInstanceOf[InfiniteBiallelicCoalescentInterval])

  }

  implicit def mu[D[X], I[X], B, M, Π, Θ]: Lens[BiallelicCoalescentLikelihood[D, I, B, M, Π, Θ], Double @@ M] = Lens[BiallelicCoalescentLikelihood[D, I, B, M, Π, Θ], Double @@ M](_.mu)(mu => _.updated(mu = mu))

  implicit def piRed[D[X], I[X], B, M, Π, Θ]: Lens[BiallelicCoalescentLikelihood[D, I, B, M, Π, Θ], Double @@ Π] = Lens[BiallelicCoalescentLikelihood[D, I, B, M, Π, Θ], Double @@ Π](_.piRed)(piRed => _.updated(piRed = piRed))

  implicit def atNe[D[X], I[X], B, M, Π, Θ]: At[BiallelicCoalescentLikelihood[D, I, B, M, Π, Θ], Int, Double @@ Θ] = new At[BiallelicCoalescentLikelihood[D, I, B, M, Π, Θ], Int, Double @@ Θ] {
    override def at(i: Int): Lens[BiallelicCoalescentLikelihood[D, I, B, M, Π, Θ], @@[Double, Θ]] = {
      Lens[BiallelicCoalescentLikelihood[D, I, B, M, Π, Θ], Double @@ Θ](_.coalIntervals(i).Ne)(Ne => { bcl =>
        implicit val distributed = bcl.distributed
        import distributed._
        val coalRate = 1 / Ne
        val coalIntervals = bcl.coalIntervals.updated(i, bcl.coalIntervals(i).copy(Ne = Ne))
        val broadcastedInfiniteInterval = if (distributed.retrieve(bcl.infiniteInterval).coalIndex == i) distributed.broadcast(distributed.retrieve(bcl.infiniteInterval).updatedCoalRate(coalRate)) else bcl.infiniteInterval
        val greenBound = bcl.greenBound.updatedCoalRate(i, coalRate, distributed.retrieve(broadcastedInfiniteInterval))
        val redBound = bcl.redBound.updatedCoalRate(i, coalRate, distributed.retrieve(broadcastedInfiniteInterval))
        val greenP = greenBound.evaluate
        val redP = redBound.evaluate
        val lights = bcl.lights.map { light =>
          val bound = if (light.lower.red)
            new Lower(redP, light.lower.q, true)
          else
            new Lower(greenP, light.lower.q, false)
          new DatumLikelihood(light.lit, light.probability.updatedCoalRate(i, coalRate, distributed.retrieve(broadcastedInfiniteInterval)), bound)
        }.persist()
        new BiallelicCoalescentLikelihood[D, I, B, M, Π, Θ](lights, greenBound, redBound, bcl.mu, bcl.piRed, broadcastedInfiniteInterval, coalIntervals, bcl.datum, bcl.greenData, bcl.redData, bcl.age + 1)
      })
    }
  }

  implicit def lights[D[X], I[X], B, M, Π, Θ]: Lens[BiallelicCoalescentLikelihood[D, I, B, M, Π, Θ], D[DatumLikelihood[B, BiallelicSiteLikelihood, Lower]]] =
    Lens[BiallelicCoalescentLikelihood[D, I, B, M, Π, Θ], D[DatumLikelihood[B, BiallelicSiteLikelihood, Lower]]](_.lights)(lights => bcl => new BiallelicCoalescentLikelihood(lights, bcl.greenBound, bcl.redBound, bcl.mu, bcl.piRed, bcl.infiniteInterval, bcl.coalIntervals, bcl.datum, bcl.greenData, bcl.redData, bcl.age + 1)(bcl.distributed))

}
