package apg

final class NativePointer(val value: Long) {

  override def finalize(): Unit = {
    NativeLib.free(value)
    super.finalize()
  }

}
