import apg.BiallelicCoalescentLikelihood.IBCI
import org.apache.spark.SparkContext
import org.apache.spark.broadcast.Broadcast
import org.apache.spark.rdd.RDD
import snap.FMatrix
import snap.matrix.QMatrix

import scala.collection.parallel.mutable.ParArray
import scala.reflect.ClassTag

package object apg {

  type F = FMatrix
  type Q = QMatrix

  implicit object ArrayIsDistributed extends Distributed[ParArray] {

    override def head[A](a: ParArray[A]): A = a.head

    override def map[A, B : ClassTag](a: ParArray[A])(f: (A) => B): ParArray[B] = a.map(f)

    override def synchronizedMap[A, B : ClassTag, T](a: ParArray[A])(f: (Int) => T)(g: T => A => B): ParArray[B] = {
      val t = f(0)
      a.map(g(t))
    }

    override def zipMap[A, B : ClassTag, T : ClassTag](a1: ParArray[A], a2: ParArray[B])(f: (A, B) => T): ParArray[T] =
      (a1.seq, a2.seq).zipped.map(f).par

    override def zipWithIndexMap[A, B : ClassTag](a: ParArray[A])(f: (A, Long) => B): ParArray[B] =
      a.zipWithIndex.map(x => f(x._1, x._2))

    def size[A](a: ParArray[A]): Long = a.length

    override def sum(a: ParArray[Double]): Double = a.sum

    override def checkpoint[A](c: ParArray[A]): Unit = ()

  }

  implicit object RDDIsDistributed extends Distributed[RDD] {

    override def head[A](rdd: RDD[A]): A = rdd.first()

    override def map[A, B : ClassTag](rdd: RDD[A])(f: (A) => B): RDD[B] = rdd.map(f).persist()

    def synchronizedMap[A, B : ClassTag, T](rdd: RDD[A])(f: Int => T)(g: T => A => B): RDD[B] =
      rdd.mapPartitionsWithIndex { (i, it) =>
        val t = f(i)
        it.map(g(t))
      }

    override def zipMap[A, B : ClassTag, T : ClassTag](rdd1: RDD[A], rdd2: RDD[B])(f: (A, B) => T): RDD[T] =
      rdd1.zip(rdd2).map(f.tupled)

    override def zipWithIndexMap[A, B : ClassTag](rdd: RDD[A])(f: (A, Long) => B): RDD[B] =
      rdd.zipWithIndex.map(f.tupled)

    def size[A](rdd: RDD[A]): Long = rdd.count

    override def sum(rdd: RDD[Double]): Double = rdd.sum

    override def checkpoint[A](c: RDD[A]): Unit = c.localCheckpoint()

  }

  implicit def broadcastIsIBCI(implicit sc: SparkContext): IBCI[Broadcast[InfiniteBiallelicCoalescentInterval]] = new IBCI[Broadcast[InfiniteBiallelicCoalescentInterval]] {

    override def apply(interval: InfiniteBiallelicCoalescentInterval): Broadcast[InfiniteBiallelicCoalescentInterval] = sc.broadcast(interval)

    override def unapply(t: Broadcast[InfiniteBiallelicCoalescentInterval]): InfiniteBiallelicCoalescentInterval = t.value

  }

}
