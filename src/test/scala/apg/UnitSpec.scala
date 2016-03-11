package org.compevol.apg

import org.scalactic.{Equality, TolerantNumerics}
import org.scalatest.WordSpec

import scala.annotation.tailrec

abstract class UnitSpec extends WordSpec {

  implicit object NumericalEquivalence extends Equality[Double] {

    val epsilon = {
      @tailrec def recurse(eps: Double = 1.0): Double = if (1.0 + eps == 1.0) 2 * eps else recurse(eps / 2.0)
      recurse()
    }
    val sqrtEpsilon = math.sqrt(epsilon)

    override def areEqual(a: Double, b: Any): Boolean = b match {
      case b: Double => math.abs((a / b) - 1) <= sqrtEpsilon
      case _ => false
    }

  }

}
