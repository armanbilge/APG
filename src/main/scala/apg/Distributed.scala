package apg

import scala.language.higherKinds
import scala.reflect.ClassTag

trait Distributed[D[X], B[X]] {

  def head[A](d: D[A]): A

  def range(start: Int, end: Int): D[Int]

  def map[A, B : ClassTag](d: D[A])(f: A => B): D[B]

  def filter[A](d: D[A])(f: A => Boolean): D[A]

  def partitionAwareMap[A, B : ClassTag, T](d: D[A])(f: Int => T)(g: T => A => B): D[B]

  def zipMap[A, B : ClassTag, T : ClassTag](d1: D[A], d2: D[B])(f: (A, B) => T): D[T]

  def zipWithIndexMap[A, B : ClassTag](d: D[A])(f: (A, Long) => B): D[B]

  def size[A](d: D[A]): Long

  def sum(d: D[Double]): Double

  def persist[A](d: D[A]): D[A]

  def checkpoint[A](d: D[A]): Unit

  def broadcast[A : ClassTag](a: A): B[A]

  def retrieve[A](b: B[A]): A

  implicit class Ops[A](val d: D[A])(implicit distributed: Distributed[D, B]) {
    def head: A = distributed.head(d)
    def map[B : ClassTag](f: A => B): D[B] = distributed.map(d)(f)
    def filter(f: A => Boolean): D[A] = distributed.filter(d)(f)
    def synchronizedMap[B : ClassTag, T](f: Int => T)(g: T => A => B): D[B] = distributed.partitionAwareMap(d)(f)(g)
    def zipMap[B : ClassTag, T : ClassTag](d2: D[B])(f: (A, B) => T): D[T] = distributed.zipMap(d, d2)(f)
    def zipWithIndexMap[B : ClassTag](f: (A, Long) => B): D[B] = distributed.zipWithIndexMap(d)(f)
    def size: Long = distributed.size(d)
    def persist(): D[A] = distributed.persist(d)
    def checkpoint(): Unit = distributed.checkpoint(d)
  }

  implicit class DoubleOps[A](val d: D[Double])(implicit distributed: Distributed[D, B]) {
    def sum: Double = distributed.sum(d)
  }

}

object Distributed {

  @inline def apply[D[X], B[X]](implicit d: Distributed[D, B]): Distributed[D, B] = d

}
