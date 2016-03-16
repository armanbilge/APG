package org.compevol.apg

package object hky {

  sealed trait Substitution
  object Transition extends Substitution
  object Transversion extends Substitution

}
