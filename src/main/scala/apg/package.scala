import snap.FMatrix
import snap.matrix.QMatrix

import scala.collection.parallel.mutable.ParArray
import scala.reflect.ClassTag

package object apg {

  type F = FMatrix
  type Q = QMatrix

  implicit object ArrayIsDistributed extends Distributed[ParArray, Some] {

    override def head[A](a: ParArray[A]): A = a.head

    override def range(start: Int, end: Int): ParArray[Int] = (start until end).toParArray

    override def map[A, B : ClassTag](a: ParArray[A])(f: (A) => B): ParArray[B] = a.map(f)

    override def filter[A](a: ParArray[A])(f: (A) => Boolean): ParArray[A] = a.filter(f)

    override def partitionAwareMap[A, B : ClassTag, T](a: ParArray[A])(f: (Int) => T)(g: T => A => B): ParArray[B] = {
      val t = f(0)
      a.map(g(t))
    }

    override def zipMap[A, B : ClassTag, T : ClassTag](a1: ParArray[A], a2: ParArray[B])(f: (A, B) => T): ParArray[T] =
      (a1.seq, a2.seq).zipped.par.map(f.tupled).to

    override def zipWithIndexMap[A, B : ClassTag](a: ParArray[A])(f: (A, Long) => B): ParArray[B] =
      a.zipWithIndex.map(x => f(x._1, x._2))

    def size[A](a: ParArray[A]): Long = a.length

    override def sum(a: ParArray[Double]): Double = a.sum

    override def persist[A](a: ParArray[A]): ParArray[A] = a

    override def checkpoint[A](a: ParArray[A]): Unit = ()

    override def broadcast[A : ClassTag](a: A): Some[A] = Some(a)

    override def retrieve[A](s: Some[A]): A = s.x

  }

}
