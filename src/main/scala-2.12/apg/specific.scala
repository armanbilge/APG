package apg

import scala.collection.parallel.mutable.ParArray

object specific {

  def run(args: Array[String]): Unit = Main.run[ParArray, Some](args(0), rng => _ => rng)

}
