import snap.FMatrix
import snap.matrix.QMatrix

import scala.collection.parallel.mutable.ParArray
import scala.reflect.ClassTag

package object apg {

  implicit object ArrayIsDistributed extends Distributed[Array, Some] {

    override def head[A](a: Array[A]): A = genericArrayOps(a).head

    override def range(start: Int, end: Int): Array[Int] = (start until end).toArray

    override def map[A, B : ClassTag](a: Array[A])(f: (A) => B): Array[B] = genericArrayOps(a).map(f)

    override def filter[A](a: Array[A])(f: (A) => Boolean): Array[A] = genericArrayOps(a).filter(f)

    override def partitionAwareMap[A, B : ClassTag, T](a: Array[A])(f: (Int) => T)(g: T => A => B): Array[B] = {
      val t = f(0)
      genericArrayOps(a).map(g(t))
    }

    override def zipMap[A, B : ClassTag, T : ClassTag](a1: Array[A], a2: Array[B])(f: (A, B) => T): Array[T] =
      (a1, a2).zipped.map(f)

    override def zipWithIndexMap[A, B : ClassTag](a: Array[A])(f: (A, Long) => B): Array[B] =
      genericArrayOps(a).zipWithIndex.map(x => f(x._1, x._2))

    def size[A](a: Array[A]): Long = a.length

    override def sum(a: Array[Double]): Double = genericArrayOps(a).sum

    override def persist[A](a: Array[A]): Array[A] = a

    override def checkpoint[A](a: Array[A]): Unit = ()

    override def broadcast[A : ClassTag](a: A): Some[A] = Some(a)

    override def retrieve[A](s: Some[A]): A = s.value

  }

  implicit object ParArrayIsDistributed extends Distributed[ParArray, Some] {

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

    override def retrieve[A](s: Some[A]): A = s.value

  }

}
