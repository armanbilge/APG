package org.compevol.apg

import java.io.File

import org.compevol.mcmc.OneOnXPrior

object Main extends App {

//  val timepoints = args.grouped(2).map(a => TimePoint.fromFile(a(0).toDouble, new File(a(1)))).toSeq
//  val sites = timepoints.head.indices.map(i => timepoints.map(_ (i)).sorted.toList).filter(_.forall(_.redCountPartial.exists(_ > 1E-20)))
//
//  val bcl = new BiallelicCoalescentLikelihood(sites)
//  def like(mu: Double, Ne: Double) = bcl(mu, 0.5, List(CoalescentInterval(Double.PositiveInfinity, Ne)))
//
//  def prior(mu: Double, Ne: Double) = OneOnXPrior(Ne)
//
//  def post(mu: Double, Ne: Double) = like(mu, Ne) + prior(mu, Ne)

}
