package apg

import java.io.{File, PrintWriter}

import mcmc.AutoTuningMCMC.OperatorState
import mcmc._
import mcmc.implicits._
import monocle.function.At
import net.jcazevedo.moultingyaml._
import shapeless.tag
import shapeless.tag.@@
import spire.random.Generator
import spire.random.rng.MersenneTwister64
import spire.std.double._

import scala.collection.parallel.mutable.ParArray
import scala.io.Source
import scala.language.{existentials, higherKinds}

object Main extends App {

  specific.run(args)

  def run[D[X], B[X]](fn: String, rngGen: Generator => Array[Long] => Generator)(implicit distributed: Distributed[D, B]): Unit = {

    import distributed._

    case class Datum(age: Double, file: String)
    case class Interval(length: Double, theta: Double)
    case class Partition(mu: Double, data: List[Datum])
    case class Config(partitions: List[Partition], intervals: List[Interval], length: Int, frequency: Int, tuningDelay: Option[Int], seed: Option[Long])

    object YamlProtocol extends DefaultYamlProtocol {
      implicit val datumFormat = yamlFormat2(Datum)
      implicit val intervalFormat = yamlFormat2(Interval)
      implicit val partitionFormat = yamlFormat2(Partition)
      implicit val configFormat = yamlFormat6(Config)
    }
    import YamlProtocol._

    val src = Source.fromFile(fn)
    val config = src.mkString.parseYaml.convertTo[Config]
    src.close()

    implicit val rng = MersenneTwister64.fromTime(config.seed.getOrElse(System.nanoTime())).sync

    trait Theta
    trait Mu
    trait X

    val partitions = config.partitions.map { case Partition(mu, data) => (mu, Sites[D, B](data.map(d => TimePoint.fromFile(d.age, new File(d.file))))) }
    val intervals = config.intervals.map(x => CoalescentInterval(x.length, tag[Theta](x.theta)))

    val partitionWeights = {
      val siteCounts = partitions.map { case (_, sites) => sites.size }
      val total = siteCounts.sum.toDouble
      siteCounts.map(_ / total).toArray
    }

    def weightedMean(xs: Traversable[Double]): Double = (xs, partitionWeights).zipped.map(_ * _).sum

    type Li = BiallelicCoalescentLikelihood[D, B, X, Mu, Theta]
    type L = IIDProbability[Double, Li, IndexedSeq[Li]]
    type Pr = ExponentialMarkovPrior[Double @@ Theta, Double @@ X, IndexedSeq[Double @@ Theta]]
    type P = JointProbability[Double, L, Pr]
    val like: L = new L(partitions.toArray.map { case (mu, sites) => BiallelicCoalescentLikelihood[D, B, X, Mu, Theta](tag[Mu](mu), intervals, sites) })
    val prior: Pr = ExponentialMarkovPrior[Double @@ Theta, Double @@ X, IndexedSeq[Double @@ Theta]](config.intervals.view.map(_.theta).map(tag[Theta](_)).toIndexedSeq, tag[X](1.0))
    val post: P = new P(like, prior)

    val _mu = partitions.indices.map(implicitly[At[P, Int, Double @@ Mu]].at).map(mcmc.implicits.untaggedLens[P, Double, Mu])
    val _theta = intervals.indices.map(implicitly[At[P, Int, Double @@ Theta]].at).map(mcmc.implicits.untaggedLens[P, Double, Theta])
    val _lights = implicitly[At[P, Int, D[DatumLikelihood[X, BiallelicSiteLikelihood, BiallelicSiteLikelihood, BiallelicCoalescentLikelihood.Lower]]]]

    val scaleFactor = 0.75
    val estimateMu = config.partitions.head.data.size > 1
    val muScalers = _mu.map(x => AutoTuningMCMC.statify[P, Double, ScaleOperator[P, Double]](new ScaleOperator[P, Double](scaleFactor, Traversable(x)), if (estimateMu) 1.0 else 0.0))
    val muScaler = AutoTuningMCMC.statify[P, Double, ScaleOperator[P, Double]](new ScaleOperator[P, Double](scaleFactor, _mu), if (estimateMu) 1.0 else 0.0)
    val thetaScalers = _theta.map(x => AutoTuningMCMC.statify[P, Double, ScaleOperator[P, Double]](new ScaleOperator[P, Double](scaleFactor, Traversable(x)), 1.0)) ++
      _theta.sliding(2).map(x => AutoTuningMCMC.statify[P, Double, ScaleOperator[P, Double]](new ScaleOperator[P, Double](scaleFactor, x), 2.0))
    val thetaScaler = AutoTuningMCMC.statify[P, Double, ScaleOperator[P, Double]](new ScaleOperator[P, Double](scaleFactor, _theta), _theta.length)
    val upDownOp = if (estimateMu) Some(AutoTuningMCMC.statify[P, Double, UpDownOperator[P, Double]](new UpDownOperator[P, Double](scaleFactor, _theta, _mu), _theta.length + 1.0)) else None
    val ffos = partitions.indices.map { i =>
      val tweak = like.ps(i).lights.asInstanceOf[ParArray[DatumLikelihood[X, BiallelicSiteLikelihood, BiallelicSiteLikelihood, BiallelicCoalescentLikelihood.Lower]]].map(d => (math.exp(d.entropy) - 1) / 2)
      val ffo = AutoTuningMCMC.statify[P, Double, FocusedOperator[P, D[DatumLikelihood[X, BiallelicSiteLikelihood, BiallelicSiteLikelihood, BiallelicCoalescentLikelihood.Lower]], Double, FireFlyOperator[D, B, X, BiallelicSiteLikelihood, BiallelicSiteLikelihood, BiallelicCoalescentLikelihood.Lower]]](new FocusedOperator[P, D[DatumLikelihood[X, BiallelicSiteLikelihood, BiallelicSiteLikelihood, BiallelicCoalescentLikelihood.Lower]], Double, FireFlyOperator[D, B, X, BiallelicSiteLikelihood, BiallelicSiteLikelihood, BiallelicCoalescentLikelihood.Lower]](new FireFlyOperator[D, B, X, BiallelicSiteLikelihood, BiallelicSiteLikelihood, BiallelicCoalescentLikelihood.Lower](i => tweak.apply(i.toInt)), _lights.at(i)), 1)
      ffo
    }

    val pw = new PrintWriter(fn + ".log")
    pw.println((Traversable("state", "joint", "likelihood", "prior", "mu") ++ partitions.indices.map("mu_" + _) ++ intervals.indices.map("theta_" + _) ++ partitions.indices.flatMap(i => Traversable(s"on_$i", s"dim_$i"))).mkString("\t"))
    AutoTuningMCMC.chain[Double, P](post, config.tuningDelay.getOrElse(config.length / 100), IndexedSeq[OperatorState[P, Double, O] forSome {type O <: Operator[P, Double]}](muScaler, thetaScaler) ++ upDownOp ++ muScalers ++ thetaScalers ++ ffos).toIterable.take(config.length + 1).zipWithIndex.filter(_._2 % config.frequency == 0).foreach { case ((state, _), i) =>
      println((Traversable[Any](i, state.evaluate, state.p.evaluate, state.q.evaluate, weightedMean(_mu.map(_.get(state)))) ++ _theta.map(_.get(state))).mkString("\t"))
      pw.println((Traversable[Any](i, state.evaluate, state.p.evaluate, state.q.evaluate, weightedMean(_mu.map(_.get(state)))) ++ _mu.map(_.get(state)) ++ _theta.map(_.get(state)) ++ state.p.ps.flatMap(l => Traversable(l.fractionOn, l.fractionDim))).mkString("\t"))
      pw.flush()
    }

  }

}
