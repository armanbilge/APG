package apg

import org.apache.spark.{SparkConf, SparkContext}
import org.apache.spark.broadcast.Broadcast
import org.apache.spark.rdd.{DoubleRDDFunctions, RDD}
import spire.random.rng.MersenneTwister64

import scala.reflect.ClassTag

object specific {

  val sc = new SparkContext(new SparkConf()
    .setAppName("apg")
    .set("spark.serializer", "org.apache.spark.serializer.KryoSerializer")
    .registerKryoClasses(Array(classOf[TimePoint], classOf[Array[TimePoint]], classOf[Array[Array[TimePoint]]], classOf[InfiniteBiallelicCoalescentInterval]))
  )

  def run(args: Array[String]): Unit = {
    sc.setCheckpointDir(args(1))
    Main.run[RDD, Broadcast](args(0), _ => x => MersenneTwister64.fromArray(x))
  }

  implicit def rddIsDistributed: Distributed[RDD, Broadcast] = new RDDIsDistributed()

  class RDDIsDistributed extends Distributed[RDD, Broadcast] with Serializable {

    override def head[A](rdd: RDD[A]): A = rdd.first()

    override def range(start: Int, end: Int): RDD[Int] = sc.range(start, end).map(_.toInt)

    override def map[A, B : ClassTag](rdd: RDD[A])(f: (A) => B): RDD[B] = rdd.map(f)

    override def filter[A](rdd: RDD[A])(f: (A) => Boolean): RDD[A] = rdd.filter(f)

    def partitionAwareMap[A, B : ClassTag, T](rdd: RDD[A])(f: Int => T)(g: T => A => B): RDD[B] =
      rdd.mapPartitionsWithIndex { (i, it) =>
        val t = f(i)
        it.map(g(t))
      }

    override def zipMap[A, B : ClassTag, T : ClassTag](rdd1: RDD[A], rdd2: RDD[B])(f: (A, B) => T): RDD[T] =
      rdd1.zip(rdd2).map(f.tupled)

    override def zipWithIndexMap[A, B : ClassTag](rdd: RDD[A])(f: (A, Long) => B): RDD[B] =
      rdd.zipWithIndex.map(f.tupled)

    def size[A](rdd: RDD[A]): Long = rdd.count

    override def sum(rdd: RDD[Double]): Double = new DoubleRDDFunctions(rdd).sum

    override def persist[A](rdd: RDD[A]): RDD[A] = rdd.persist()

    override def checkpoint[A](rdd: RDD[A]): Unit = rdd.checkpoint()

    override def broadcast[A : ClassTag](a: A): Broadcast[A] = sc.broadcast(a)

    override def retrieve[A](b: Broadcast[A]): A = b.value

  }

}
