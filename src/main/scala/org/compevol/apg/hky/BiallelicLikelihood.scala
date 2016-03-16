package org.compevol.apg.hky

import org.compevol.apg._

import scala.collection.LinearSeq

class BiallelicLikelihood(val taxa: Set[Taxon], val mu: Double, val kappa: Double, val pi: IndexedSeq[Double], val coalescentIntervals: LinearSeq[CoalescentInterval], val errorProbs: Taxon => Double) extends (Map[Site, Double] => Double) {

  override def apply(columns: Map[Site, Double]): Double = columns.view.par.map(Function.tupled({ (site, w) =>

    val samples = taxa.view.groupBy(_.height).map(Function.tupled({ (t, tx) =>
      BiallelicSample(t, site match {
        case ConstantSite(_) => tx.view.map(errorProbs).toSeq
        case TwoStateSite(_, _, f) => tx.view.map(t => if (f(t)) 1 - errorProbs(t) else errorProbs(t)).toSeq
      })
    })).to[LinearSeq].sorted

    // TODO
//    val L = (site match {
//      case ConstantSite(n) => (Nucleotide.stateSet - n).view.map((_, n))
//      case TwoStateSite(red, green, _) => Traversable((red, green))
//    }).map(Function.tupled { (red, green) =>
//      def freq(n: Nucleotide) = Nucleotide.unambiguous(n).map()
//    })

    0.0

  })).sum

}
