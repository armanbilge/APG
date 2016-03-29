package org.compevol.apg.hky

import org.compevol.apg.{Nucleotide, Taxon}

import java.io.File
import scala.io.Source

sealed trait Site

object Site {

  def fromFile(file: File, taxa: Seq[Taxon]): Map[Site, Double] = Source.fromFile(file).getLines().map(_.split("\t").toList match { case p :: w :: Nil => (p, w.toDouble, Set(p: _*).toList) }).collect {
    case (p, w, i :: Nil) => ConstantSite(Nucleotide(i)) -> w
    case (p, w, i :: j :: Nil) => TwoStateSite(Nucleotide(i), Nucleotide(j), taxa.zip(p).map(Function.tupled((t, k) => (t, k == j))).toMap) -> w
  }.toMap

}

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
