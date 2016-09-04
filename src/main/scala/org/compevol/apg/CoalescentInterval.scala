package org.compevol.apg

case class CoalescentInterval(length: Double, Ne: Double) {

  require(length >= 0)
  require(Ne > 0)

}
