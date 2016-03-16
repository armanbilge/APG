package org.compevol.apg.hky

import org.compevol.apg._

import scala.collection.LinearSeq

class BiallelicLikelihood(val taxa: Set[Taxon], val mu: Double, val kappa: Double, val pi: Map[Nucleotide, Double], val coalescentIntervals: LinearSeq[CoalescentInterval], val errorProbs: Taxon => Double) extends (Map[AlignmentColumn, Double] => Double) {


  override def apply(columns: Map[AlignmentColumn, Double]): Double = columns.view.par.map(Function.tupled({ (col, w) =>

    val samples = taxa.view.groupBy(_.height).map(Function.tupled({ (t, tx) =>
      BiallelicSample(t, tx.view.map { t =>
        col.sites(t) match {
          case false => errorProbs(t)
          case true => 1 - errorProbs(t)
        }
      }.toSeq)
    })).to[LinearSeq].sorted

    val red = col.red
    val L = col.green match {
      case Some(green) =>
        val piRed = Nucleotide.unambiguous(red).map(pi).sum
        val piGreen = Nucleotide.unambiguous(green).map(pi).sum
        (new BiallelicCoalescentLikelihood(mu, (piRed, piGreen), coalescentIntervals))(samples)
      case None =>
        0.0
    }

    w * L

  })).sum

}
