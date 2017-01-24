package apg

import monocle.Lens
import shapeless.tag.@@

case class CoalescentInterval[T](length: Double, Ne: Double @@ T) {

  require(length >= 0)
  require(Ne > 0)

}

object CoalescentInterval {

  implicit def Ne[T]: Lens[CoalescentInterval[T], Double @@ T] =
    Lens[CoalescentInterval[T], Double @@ T](_.Ne)(Ne => _.copy(Ne = Ne))

}
