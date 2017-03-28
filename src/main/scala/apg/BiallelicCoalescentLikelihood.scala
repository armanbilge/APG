package apg

import apg.BiallelicCoalescentLikelihood.Lower
import mcmc.Probability
import monocle.Lens
import monocle.function.At
import org.apache.spark.SparkContext
import org.apache.spark.broadcast.Broadcast
import org.apache.spark.rdd.RDD
import shapeless.tag.@@

import scala.annotation.tailrec
import scala.collection.LinearSeq

class BiallelicCoalescentLikelihood[B, M, Π, Θ](val lights: RDD[DatumLikelihood[B, BiallelicSiteLikelihood, Lower]],
                                                val greenBound: BiallelicSiteLikelihood,
                                                val redBound: BiallelicSiteLikelihood,
                                                val mu: Double @@ M,
                                                val piRed: Double @@ Π,
                                                val infiniteInterval: Broadcast[InfiniteBiallelicCoalescentInterval],
                                                val coalIntervals: LinearSeq[CoalescentInterval[Θ]],
                                                val datum: LinearSeq[TimePoint],
                                                val greenData: LinearSeq[Int => Double],
                                                val redData: LinearSeq[Int => Double],
                                                val age: Long
                                               )(implicit val sc: SparkContext) extends Probability[Double] {

  if (age % 50 == 0) lights.checkpoint()

  lazy val evaluate: Double = lights.map(_.evaluate).sum

  lazy val fractionLit: Double = lights.map(l => if (l.lit) 1 else 0).sum / lights.count()

  def updated(mu: Double @@ M = mu, piRed: Double @@ Π = piRed): BiallelicCoalescentLikelihood[B, M, Π, Θ] = {

    val (intervals, infiniteInterval) = BiallelicCoalescentLikelihood.createIntervals(mu, piRed, coalIntervals, datum)
    val broadcastedInfiniteInterval = sc.broadcast(infiniteInterval)
    val greenBound = BiallelicSiteLikelihood(piRed, broadcastedInfiniteInterval, intervals, greenData)
    val redBound = BiallelicSiteLikelihood(piRed, broadcastedInfiniteInterval, intervals, redData)
    val greenP = greenBound.evaluate
    val redP = redBound.evaluate
    val lights = this.lights.map { light =>
      val bound = if (light.lower.red)
        new Lower(redP, light.lower.q, true)
      else
        new Lower(greenP, light.lower.q, false)
      new DatumLikelihood(light.lit, BiallelicSiteLikelihood(piRed, broadcastedInfiniteInterval, intervals, light.probability.partials), bound)
    }.persist()
    new BiallelicCoalescentLikelihood(lights, greenBound, redBound, mu, piRed, broadcastedInfiniteInterval, coalIntervals, datum, greenData, redData, age + 1)

  }

}

object BiallelicCoalescentLikelihood {

  def apply[B, M, Π, Θ](lit: IndexedSeq[Boolean @@ B], mu: Double @@ M, piRed: Double @@ Π, coalIntervals: LinearSeq[CoalescentInterval[Θ]], data: RDD[LinearSeq[TimePoint]], init: Boolean = false)(implicit sc: SparkContext): BiallelicCoalescentLikelihood[B, M, Π, Θ] = {

    val first = data.first()
    val greenData = List.fill(first.size)((i: Int) => if (i == 0) 1.0 else 0.0)
    val redData = first.map(_.redCountPartial.length - 1).map(k => (i: Int) => if (i == k) 1.0 else 0.0)
    val (intervals, infiniteInterval) = createIntervals(mu, piRed, coalIntervals, data.first)
    val broadcastedInfiniteInterval = sc.broadcast(infiniteInterval)
    val greenBound = BiallelicSiteLikelihood(piRed, broadcastedInfiniteInterval, intervals, greenData)
    val redBound = BiallelicSiteLikelihood(piRed, broadcastedInfiniteInterval, intervals, redData)
    val greenP = greenBound.evaluate
    val redP = redBound.evaluate
    val lights = data.zip(sc.parallelize(lit)).map( Function.tupled { (sample, lit) =>
      val partials = sample.map(_.redCountPartial)
      val like = BiallelicSiteLikelihood(piRed, broadcastedInfiniteInterval, intervals, partials.toList)
      val greenScaler = partials.map(_.head).product
      val redScaler = partials.map(_.last).product
      val bound = if (greenScaler > redScaler)
        new Lower(greenP, greenScaler, false)
      else
        new Lower(redP, redScaler, true)
      val dl = new DatumLikelihood[B, BiallelicSiteLikelihood, Lower](lit, like, bound)
      if (init && dl.evaluate.isNegInfinity)
        dl.flipped
      else
        dl
    }).persist()
    new BiallelicCoalescentLikelihood[B, M, Π, Θ](lights, greenBound, redBound, mu, piRed, broadcastedInfiniteInterval, coalIntervals, first, greenData, redData, 0)

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

    val intervals = recurse(coalIntervals, samples, coalIntervals.head.length)
    (intervals.reverse, intervals.head.asInstanceOf[InfiniteBiallelicCoalescentInterval])

  }

  implicit def mu[B, M, Π, Θ]: Lens[BiallelicCoalescentLikelihood[B, M, Π, Θ], Double @@ M] = Lens[BiallelicCoalescentLikelihood[B, M, Π, Θ], Double @@ M](_.mu)(mu => _.updated(mu = mu))

  implicit def piRed[B, M, Π, Θ]: Lens[BiallelicCoalescentLikelihood[B, M, Π, Θ], Double @@ Π] = Lens[BiallelicCoalescentLikelihood[B, M, Π, Θ], Double @@ Π](_.piRed)(piRed => _.updated(piRed = piRed))

  implicit def atNe[B, M, Π, Θ]: At[BiallelicCoalescentLikelihood[B, M, Π, Θ], Int, Double @@ Θ] = new At[BiallelicCoalescentLikelihood[B, M, Π, Θ], Int, Double @@ Θ] {
    override def at(i: Int): Lens[BiallelicCoalescentLikelihood[B, M, Π, Θ], @@[Double, Θ]] = {
      Lens[BiallelicCoalescentLikelihood[B, M, Π, Θ], Double @@ Θ](_.coalIntervals(i).Ne)(Ne => { bcl =>
        val coalRate = 1 / Ne
        val coalIntervals = bcl.coalIntervals.updated(i, bcl.coalIntervals(i).copy(Ne = Ne))
        val broadcastedInfiniteInterval = if (bcl.infiniteInterval.value.coalIndex == i) bcl.sc.broadcast(bcl.infiniteInterval.value.updatedCoalRate(coalRate)) else bcl.infiniteInterval
        val greenBound = bcl.greenBound.updatedCoalRate(i, coalRate, broadcastedInfiniteInterval)
        val redBound = bcl.redBound.updatedCoalRate(i, coalRate, broadcastedInfiniteInterval)
        val greenP = greenBound.evaluate
        val redP = redBound.evaluate
        val lights = bcl.lights.map { light =>
          val bound = if (light.lower.red)
            new Lower(redP, light.lower.q, true)
          else
            new Lower(greenP, light.lower.q, false)
          new DatumLikelihood(light.lit, light.probability.updatedCoalRate(i, coalRate, broadcastedInfiniteInterval), bound)
        }.persist()
        new BiallelicCoalescentLikelihood[B, M, Π, Θ](lights, greenBound, redBound, bcl.mu, bcl.piRed, broadcastedInfiniteInterval, coalIntervals, bcl.datum, bcl.greenData, bcl.redData, bcl.age + 1)(bcl.sc)
      })
    }
  }

  implicit def lights[B, M, Π, Θ]: Lens[BiallelicCoalescentLikelihood[B, M, Π, Θ], RDD[DatumLikelihood[B, BiallelicSiteLikelihood, Lower]]] =
    Lens[BiallelicCoalescentLikelihood[B, M, Π, Θ], RDD[DatumLikelihood[B, BiallelicSiteLikelihood, Lower]]](_.lights)(lights => bcl => new BiallelicCoalescentLikelihood(lights, bcl.greenBound, bcl.redBound, bcl.mu, bcl.piRed, bcl.infiniteInterval, bcl.coalIntervals, bcl.datum, bcl.greenData, bcl.redData, bcl.age + 1)(bcl.sc))

}
