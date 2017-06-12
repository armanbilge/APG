package apg

import com.esotericsoftware.kryo.Kryo
import com.twitter.chill.{KryoSerializer, WrappedArraySerializer}
import org.apache.spark.serializer.KryoRegistrator

import scala.collection.{LinearSeq, mutable}

class MyKryoRegistrator extends KryoRegistrator {

  override def registerClasses(kryo: Kryo): Unit = {
    KryoSerializer.registerAll(kryo)
    kryo.register(classOf[Class[_]])
    kryo.register(classOf[Range])
    kryo.register(classOf[mutable.ArraySeq[_]])
    kryo.register(classOf[mutable.WrappedArray.ofRef[_]])
    kryo.register(classOf[Array[Object]])
    kryo.register(classOf[Array[Double]])
    kryo.register(classOf[Array[LinearSeq[_]]])
    kryo.register(classOf[TimePoint])
    kryo.register(classOf[Array[TimePoint]])
    kryo.register(classOf[Array[Array[TimePoint]]])
    kryo.register(classOf[InfiniteBiallelicCoalescentInterval])
  }

}
