package org.compevol.apg.hky

import org.compevol.apg.{Nucleotide, Taxon}

case class AlignmentColumn(red: Nucleotide, green: Option[Nucleotide], sites: Taxon => Boolean) {

  val substitution: Option[Substitution] = (red, green) match {
    case (_, None) => None
    case (i, Some(j)) if (Set(i, j) subsetOf Nucleotide.stateSet) && i.state % 2 == j.state % 2 => Some(Transition)
    case (i, Some(j)) if (Set(i, j) subsetOf Nucleotide.stateSet) && i.state % 2 != j.state % 2 => Some(Transversion)
    case (Nucleotide.R, Some(Nucleotide.Y)) => Some(Transversion)
    case (Nucleotide.Y, Some(Nucleotide.R)) => Some(Transversion)
    case _ => throw new IllegalArgumentException
  }

}
