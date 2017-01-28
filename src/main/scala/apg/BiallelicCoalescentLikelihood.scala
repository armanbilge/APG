package apg

import apg.BiallelicCoalescentLikelihood.Scaled
import mcmc.Probability
import monocle.Lens
import monocle.function.At
import shapeless.tag.@@

import scala.annotation.tailrec
import scala.collection.LinearSeq

class BiallelicCoalescentLikelihood[B, M, Π, Θ](val lights: IndexedSeq[DatumLikelihood[B, BiallelicSiteLikelihood, Scaled]],
                                                val greenBound: BiallelicSiteLikelihood,
                                                val redBound: BiallelicSiteLikelihood,
                                                val mu: Double @@ M,
                                                val piRed: Double @@ Π,
                                                val coalIntervals: LinearSeq[CoalescentInterval[Θ]],
                                                val data: IndexedSeq[LinearSeq[TimePoint]]
                                               ) extends Probability[Double] {

  lazy val evaluate: Double = lights.par.map(_.evaluate).sum

}

object BiallelicCoalescentLikelihood {

  def apply[B, M, Π, Θ](lit: IndexedSeq[Boolean @@ B], mu: Double @@ M, piRed: Double @@ Π, coalIntervals: LinearSeq[CoalescentInterval[Θ]], data: IndexedSeq[LinearSeq[TimePoint]]): BiallelicCoalescentLikelihood[B, M, Π, Θ] = {

    val intervals = createIntervals(mu, piRed, coalIntervals, data.head)
    val greenBound = new BiallelicSiteLikelihood(piRed, BiallelicSiteLikelihood.createCachedF(intervals, List.fill(data.head.size)(i => if (i == 0) 1.0 else 0.0)))
    val redBound = new BiallelicSiteLikelihood(piRed, BiallelicSiteLikelihood.createCachedF(intervals, data.head.map(_.redCountPartial.length - 1).map(k => (i: Int) => if (i == k) 1.0 else 0.0)))
    val lights = (data, lit).zipped.map { (sample, lit) =>
      val partials = sample.map(_.redCountPartial)
      val F = BiallelicSiteLikelihood.createCachedF(intervals, partials.toList)
      val like = new BiallelicSiteLikelihood(piRed, F)
      val greenScaler = partials.map(_.head).product
      val redScaler = partials.map(_.last).product
      val bound = if (greenScaler > redScaler)
        new Scaled(greenBound, greenScaler)
      else
        new Scaled(redBound, redScaler)
      new DatumLikelihood[B, BiallelicSiteLikelihood, Scaled](lit, like, bound)
    }
    new BiallelicCoalescentLikelihood[B, M, Π, Θ](lights, greenBound, redBound, mu, piRed, coalIntervals, data)
  }

  class Scaled(val p: Probability[Double], val scale: Double) extends Probability[Double] {
    def evaluate: Double = scale * p.evaluate
  }

  def createIntervals[Θ](mu: Double, piRed: Double, coalIntervals: LinearSeq[CoalescentInterval[Θ]], samples: LinearSeq[TimePoint]): List[BiallelicCoalescentInterval] = {

    val piGreen = 1 - piRed
    val beta = 1 / (1 - piRed * piRed - piGreen * piGreen)

    val u = beta * mu * piRed
    val v = beta * mu * piGreen

    @tailrec
    def recurse(intervals: LinearSeq[CoalescentInterval[Θ]], samples: LinearSeq[TimePoint], nextCoal: Double, t: Double = 0, m: Int = 0, coalIndex: Int = 0, acc: List[BiallelicCoalescentInterval] = Nil): List[BiallelicCoalescentInterval] = {
      samples match {
        case sample :: samplesTail =>
          val interval :: intervalsTail = intervals
          math.signum(nextCoal compare sample.t) match {
            case -1 => recurse(intervalsTail, samples, nextCoal + intervalsTail.head.length, nextCoal, m, coalIndex + 1, BiallelicCoalescentInterval(nextCoal - t, m, 0, u, v, 1 / interval.Ne, coalIndex) :: acc)
            case 1 =>
              val k = sample.k
              val mp = m + k
              val nextEvent = nextCoal min samplesTail.headOption.map(_.t).getOrElse(Double.PositiveInfinity)
              recurse(intervals, samplesTail, nextCoal, nextEvent, mp, coalIndex, BiallelicCoalescentInterval(nextEvent - t, mp, k, u, v, 1 / interval.Ne, coalIndex) :: acc)
            case 0 =>
              val k = sample.k
              val mp = m + k
              recurse(intervalsTail, samplesTail, nextCoal + intervalsTail.head.length, nextCoal, mp, coalIndex + 1, BiallelicCoalescentInterval(nextCoal - t, mp, k, u, v, 1 / interval.Ne, coalIndex) :: acc)
          }
        case Nil => intervals match {
          case interval :: Nil => acc
          case interval :: intervalsTail => recurse(intervalsTail, samples, nextCoal + intervalsTail.head.length, nextCoal, m, coalIndex + 1, BiallelicCoalescentInterval(nextCoal - t, m, 0, u, v, 1 / interval.Ne, coalIndex) :: acc)
        }
      }
    }

    recurse(coalIntervals, samples, coalIntervals.head.length).reverse

  }

  implicit def mu[B, M, Π, Θ]: Lens[BiallelicCoalescentLikelihood[B, M, Π, Θ], Double @@ M] = Lens[BiallelicCoalescentLikelihood[B, M, Π, Θ], Double @@ M](_.mu)(mu => bcl => apply(bcl.lights.map(_.lit), mu, bcl.piRed, bcl.coalIntervals, bcl.data))

  implicit def piRed[B, M, Π, Θ]: Lens[BiallelicCoalescentLikelihood[B, M, Π, Θ], Double @@ Π] = Lens[BiallelicCoalescentLikelihood[B, M, Π, Θ], Double @@ Π](_.piRed)(piRed => bcl => apply(bcl.lights.map(_.lit), bcl.mu, piRed, bcl.coalIntervals, bcl.data))

  implicit def atNe[B, M, Π, Θ]: At[BiallelicCoalescentLikelihood[B, M, Π, Θ], Int, Double @@ Θ] = { (i: Int) =>
    Lens[BiallelicCoalescentLikelihood[B, M, Π, Θ], Double @@ Θ](_.coalIntervals(i).Ne)(Ne => { bcl =>
      val coalRate = 1 / Ne
      val coalIntervals = bcl.coalIntervals.updated(i, bcl.coalIntervals(i).copy(Ne = Ne))
      val greenBound = bcl.greenBound.updatedCoalRate(i, coalRate)
      val redBound = bcl.redBound.updatedCoalRate(i, coalRate)
      val lights = bcl.lights.map { light =>
        val bound = if (light.lower.p eq bcl.greenBound) greenBound else redBound
        new DatumLikelihood(light.lit, light.probability.updatedCoalRate(i, coalRate), new Scaled(bound, light.lower.scale))
      }
      new BiallelicCoalescentLikelihood[B, M, Π, Θ](lights, greenBound, redBound, bcl.mu, bcl.piRed, coalIntervals, bcl.data)
    })
  }

  implicit def lights[B, M, Π, Θ]: Lens[BiallelicCoalescentLikelihood[B, M, Π, Θ], IndexedSeq[DatumLikelihood[B, BiallelicSiteLikelihood, Scaled]]] =
    Lens[BiallelicCoalescentLikelihood[B, M, Π, Θ], IndexedSeq[DatumLikelihood[B, BiallelicSiteLikelihood, Scaled]]](_.lights)(lights => bcl => new BiallelicCoalescentLikelihood(lights, bcl.greenBound, bcl.redBound, bcl.mu, bcl.piRed, bcl.coalIntervals, bcl.data))

}
