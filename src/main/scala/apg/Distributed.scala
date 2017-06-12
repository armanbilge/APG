package apg

import scala.language.higherKinds
import scala.reflect.ClassTag

trait Distributed[D[X]] {

  def head[A](d: D[A]): A

  def map[A, B : ClassTag](d: D[A])(f: A => B): D[B]

  def synchronizedMap[A, B : ClassTag, T](d: D[A])(f: Int => T)(g: T => A => B): D[B]

  def zipMap[A, B : ClassTag, T : ClassTag](d1: D[A], d2: D[B])(f: (A, B) => T): D[T]

  def zipWithIndexMap[A, B : ClassTag](d: D[A])(f: (A, Long) => B): D[B]

  def size[A](d: D[A]): Long

  def sum(d: D[Double]): Double

  def checkpoint[A](d: D[A]): Unit

}

object Distributed {

  @inline def apply[D[X]](implicit d: Distributed[D]): Distributed[D] = d

  implicit class Ops[D[X] : Distributed, A](val d: D[A]) {
    def head: A = Distributed[D].head(d)
    def map[B : ClassTag](f: A => B): D[B] = Distributed[D].map(d)(f)
    def synchronizedMap[B : ClassTag, T](f: Int => T)(g: T => A => B): D[B] = Distributed[D].synchronizedMap(d)(f)(g)
    def zipMap[B : ClassTag, T : ClassTag](d2: D[B])(f: (A, B) => T): D[T] = Distributed[D].zipMap(d, d2)(f)
    def zipWithIndexMap[B : ClassTag](f: (A, Long) => B): D[B] = Distributed[D].zipWithIndexMap(d)(f)
    def size: Long = Distributed[D].size(d)
    def checkpoint(): Unit = Distributed[D].checkpoint(d)
  }

  implicit class DoubleOps[D[X] : Distributed](val d: D[Double]) {
    def sum: Double = Distributed[D].sum(d)
  }

}
