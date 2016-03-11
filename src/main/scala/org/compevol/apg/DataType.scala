package org.compevol.apg

trait DataType[T <: State] extends PartialFunction[String, T] {

  def apply(i: Int): T

  def stateCount: Int

  def ambiguousStateCount: Int

  def validStrings: Set[String]

  final override def isDefinedAt(x: String): Boolean = validStrings(x)

  def unknown: T

  def gap: T

  def states: Vector[T]

  def unambiguous(s: T): Set[T]

  def toString(s: T): String

  final implicit val ThisIsDataType: DataType[T] = this

}

object DataType {

  def apply[T <: State : DataType]: DataType[T] = implicitly[DataType[T]]

}

trait State extends Any {

  def state: Int

}
