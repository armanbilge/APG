package org.compevol.apg.hky

import org.compevol.apg.{Nucleotide, Taxon}

sealed trait Site

case class TwoStateSite(red: Nucleotide, green: Nucleotide, allele: Taxon => Boolean) extends Site {

  val substitution: Substitution = (red, green) match {
    case (i, j) if (Set(i, j) subsetOf Nucleotide.stateSet) && i.state % 2 == j.state % 2 => Transition
    case (i, j) if (Set(i, j) subsetOf Nucleotide.stateSet) && i.state % 2 != j.state % 2 => Transversion
    case (Nucleotide.R, Nucleotide.Y) => Transversion
    case (Nucleotide.Y, Nucleotide.R) => Transversion
    case _ => throw new IllegalArgumentException
  }

  def frequencyRed(pi: Nucleotide => Double): Double = pi(red) / (pi(red) + pi(green))

}

case class ConstantSite(nuc: Nucleotide) extends Site
