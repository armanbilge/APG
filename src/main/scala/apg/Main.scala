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
import spire.random.rng.MersenneTwister64
import spire.std.double._

import scala.collection.parallel.mutable.ParArray
import scala.io.Source
import scala.language.existentials

object Main extends App {

  implicit val rng = MersenneTwister64.fromTime().sync

  val fn = args(0)

  case class Datum(age: Double, file: String)
  case class Config(data: List[Datum], intervals: List[Double], length: Long, frequency: Int)

  object YamlProtocol extends DefaultYamlProtocol {
    implicit val datumFormat = yamlFormat2(Datum)
    implicit val configFormat = yamlFormat4(Config)
  }
  import YamlProtocol._

  val src = Source.fromFile(fn)
  val config = src.mkString.parseYaml.convertTo[Config]
  src.close()

  trait Theta
  trait Mu
  trait X

  val sites = Sites[ParArray, Some](config.data.map(d => TimePoint.fromFile(d.age, new File(d.file))))
  val intervals = config.intervals.map(CoalescentInterval(_, tag[Theta](10000.0)))
  val lit = sites.map(_ => tag[X](false)).toIndexedSeq

  type L = BiallelicCoalescentLikelihood[ParArray, Some, X, Mu, X, Theta]
  type P = JointProbability[Double, L, ExponentialMarkovPrior[Double @@ Theta, Double @@ X, IndexedSeq[Double @@ Theta]]]
  val like: L = BiallelicCoalescentLikelihood(lit, tag[Mu](1E-7), tag[X](0.5), intervals, sites, init = true)
  val prior = ExponentialMarkovPrior[Double @@ Theta, Double @@ X, IndexedSeq[Double @@ Theta]](IndexedSeq.fill(1)(tag[Theta](10000.0)), tag[X](1.0))
  val post: P = new JointProbability(like, prior)

  val _mu = implicitly[Lens[P, Double @@ Mu]]
  val _theta = intervals.indices.map(implicitly[At[P, Int, Double @@ Theta]].at).map(mcmc.implicits.untaggedLens[P, Double, Theta])
  val _lights = implicitly[Lens[P, L]] ^|-> implicitly[Lens[L, ParArray[DatumLikelihood[X, BiallelicSiteLikelihood, BiallelicCoalescentLikelihood.Lower]]]]

  val scaleFactor = 0.75
  val muScaler = AutoTuningMCMC.statify[P, Double, ScaleOperator[P, Double]](new ScaleOperator[P, Double](scaleFactor, Traversable(_mu)), 1.0)
  val thetaScalers = _theta.map(x => AutoTuningMCMC.statify[P, Double, ScaleOperator[P, Double]](new ScaleOperator[P, Double](scaleFactor, Traversable(x)), 1.0))
  val thetaScaler = AutoTuningMCMC.statify[P, Double, ScaleOperator[P, Double]](new ScaleOperator[P, Double](scaleFactor, _theta), _theta.length)
  val upDownOp = AutoTuningMCMC.statify[P, Double, UpDownOperator[P, Double]](new UpDownOperator[P, Double](scaleFactor, Traversable(_mu), _theta), _theta.length + 1)
  val ffo = AutoTuningMCMC.statify[P, Double, FocusedOperator[P, ParArray[DatumLikelihood[X, BiallelicSiteLikelihood, BiallelicCoalescentLikelihood.Lower]], Double, FireFlyOperator[ParArray, Some, X, BiallelicSiteLikelihood, BiallelicCoalescentLikelihood.Lower]]](new FocusedOperator(new FireFlyOperator[ParArray, Some, X, BiallelicSiteLikelihood, BiallelicCoalescentLikelihood.Lower](0.01, _ => rng), _lights), 1.0)

  val pw = new PrintWriter(fn + ".log")
  pw.println((Traversable("state", "posterior", "mu") ++ intervals.indices.map("theta_" + _) ++ Traversable("lit")).mkString("\t"))
  AutoTuningMCMC.chain[Double, P](post, Set[OperatorState[P, Double, O] forSome {type O <: Operator[P, Double]}](muScaler, thetaScaler, upDownOp, ffo) ++ thetaScalers).toIterable.zipWithIndex.filter(_._2 % config.frequency == 0).foreach { li =>
    println((Traversable[Any](li._2, li._1._1.evaluate, _mu.get(li._1._1)) ++ _theta.map(_.get(li._1._1)) ++ Traversable(li._1._1.p.fractionLit)).mkString("\t"))
    pw.println((Traversable[Any](li._2, li._1._1.evaluate, _mu.get(li._1._1)) ++ _theta.map(_.get(li._1._1)) ++ Traversable(li._1._1.p.fractionLit)).mkString("\t"))
    pw.flush()
  }

}
