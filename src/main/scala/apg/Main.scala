package apg

import java.io.{File, PrintWriter}

import mcmc.AutoTuningMCMC.OperatorState
import mcmc._
import mcmc.implicits._
import monocle.Lens
import monocle.function.At
import net.jcazevedo.moultingyaml._
import shapeless.tag
import shapeless.tag.@@
import spire.random.Generator
import spire.random.rng.MersenneTwister64
import spire.std.double._

import scala.io.Source
import scala.language.{existentials, higherKinds}

object Main extends App {

  specific.run(args)

  def run[D[X], B[X]](fn: String, rngGen: Generator => Array[Long] => Generator)(implicit distributed: Distributed[D, B]): Unit = {

    import distributed._

    case class Datum(age: Double, file: String)
    case class Interval(length: Double, theta: Double)
    case class Config(data: List[Datum], threshold: Double, mu: Double, intervals: List[Interval], lit: Double, initLit: Option[String], length: Int, frequency: Int, seed: Option[Long])

    object YamlProtocol extends DefaultYamlProtocol {
      implicit val datumFormat = yamlFormat2(Datum)
      implicit val intervalFormat = yamlFormat2(Interval)
      implicit val configFormat = yamlFormat9(Config)
    }
    import YamlProtocol._

    val src = Source.fromFile(fn)
    val config = src.mkString.parseYaml.convertTo[Config]
    src.close()

    implicit val rng = MersenneTwister64.fromTime(config.seed.getOrElse(System.nanoTime())).sync

    trait Theta
    trait Mu
    trait X

    val sites = Sites[D, B](config.data.map(d => TimePoint.fromFile(d.age, new File(d.file))), config.threshold)
    val intervals = config.intervals.map(x => CoalescentInterval(x.length, tag[Theta](x.theta)))
    val lit = new IndexedSeq[Boolean @@ X] with Serializable {
      override val length = sites.size.toInt
      val f = tag[X](false)
      override def apply(idx: Int) = f
    }

    type L = BiallelicCoalescentLikelihood[D, B, X, Mu, X, Theta]
    type Pr = ExponentialMarkovPrior[Double @@ Theta, Double @@ X, IndexedSeq[Double @@ Theta]]
    type P = JointProbability[Double, L, Pr]
    val like: L = BiallelicCoalescentLikelihood[D, B, X, Mu, X, Theta](lit, tag[Mu](config.mu), tag[X](0.5), intervals, sites, true)
    val prior: Pr = ExponentialMarkovPrior[Double @@ Theta, Double @@ X, IndexedSeq[Double @@ Theta]](config.intervals.view.map(_.theta).map(tag[Theta](_)).toIndexedSeq, tag[X](1.0))
    val post: P = new JointProbability(like, prior)

    val mu = implicitly[Lens[P, Double @@ Mu]]
    val _theta = intervals.indices.map(implicitly[At[P, Int, Double @@ Theta]].at).map(mcmc.implicits.untaggedLens[P, Double, Theta])
    val _lights = implicitly[Lens[P, L]] ^|-> implicitly[Lens[L, D[DatumLikelihood[X, BiallelicSiteLikelihood, BiallelicCoalescentLikelihood.Lower]]]]

    val scaleFactor = 0.75
    val estimateMu = config.data.size > 1
    val muScaler = AutoTuningMCMC.statify[P, Double, ScaleOperator[P, Double]](new ScaleOperator[P, Double](scaleFactor, Traversable(mu)), if (estimateMu) 1.0 else 0.0)
    val thetaScalers = _theta.map(x => AutoTuningMCMC.statify[P, Double, ScaleOperator[P, Double]](new ScaleOperator[P, Double](scaleFactor, Traversable(x)), 1.0))
    val thetaScaler = AutoTuningMCMC.statify[P, Double, ScaleOperator[P, Double]](new ScaleOperator[P, Double](scaleFactor, _theta), _theta.length)
    val upDownOp = AutoTuningMCMC.statify[P, Double, CoalescentUpDownOperator[P]](CoalescentUpDownOperator[P, Theta](scaleFactor, _theta, Traversable(mu), sites.head, intervals), if (estimateMu) _theta.length + 1.0 else 0.0)
    val ffo = AutoTuningMCMC.statify[P, Double, FocusedOperator[P, D[DatumLikelihood[X, BiallelicSiteLikelihood, BiallelicCoalescentLikelihood.Lower]], Double, FireFlyOperator[D, B, X, BiallelicSiteLikelihood, BiallelicCoalescentLikelihood.Lower]]](new FocusedOperator[P, D[DatumLikelihood[X, BiallelicSiteLikelihood, BiallelicCoalescentLikelihood.Lower]], Double, FireFlyOperator[D, B, X, BiallelicSiteLikelihood, BiallelicCoalescentLikelihood.Lower]](new FireFlyOperator[D, B, X, BiallelicSiteLikelihood, BiallelicCoalescentLikelihood.Lower](config.lit, rngGen(rng)), _lights), 1)

    val pw = new PrintWriter(fn + ".log")
    pw.println((Traversable("state", "posterior", "likelihood", "prior", "mu") ++ intervals.indices.map("theta_" + _) ++ Traversable("lit")).mkString("\t"))
    AutoTuningMCMC.chain[Double, P](post, config.length / 100, IndexedSeq[OperatorState[P, Double, O] forSome {type O <: Operator[P, Double]}](muScaler, thetaScaler, upDownOp, ffo) ++ thetaScalers).toIterable.take(config.length + 1).zipWithIndex.filter(_._2 % config.frequency == 0).foreach { li =>
      println((Traversable[Any](li._2, li._1._1.evaluate, li._1._1.p.evaluate, li._1._1.q.evaluate, mu.get(li._1._1)) ++ _theta.map(_.get(li._1._1)) ++ Traversable(li._1._1.p.fractionLit)).mkString("\t"))
      pw.println((Traversable[Any](li._2, li._1._1.evaluate, li._1._1.p.evaluate, li._1._1.q.evaluate, mu.get(li._1._1)) ++ _theta.map(_.get(li._1._1)) ++ Traversable(li._1._1.p.fractionLit)).mkString("\t"))
      pw.flush()
    }

  }

}
