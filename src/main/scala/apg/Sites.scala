package apg

import org.apache.spark.SparkContext
import org.apache.spark.rdd.RDD

import scala.collection.LinearSeq

object Sites {

  def apply(timePoints: Seq[Array[TimePoint]], threshold: Double = 1E-20)(implicit sc: SparkContext): RDD[LinearSeq[TimePoint]] = {
    sc.range(0, timePoints.head.length).map(_.toInt).map(i => timePoints.map(_(i)).sorted.to[LinearSeq]).filter(_.forall(_.redCountPartial.exists(_ > threshold))).persist()
  }

}
