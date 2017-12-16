package apg

final class NativePointer(val value: Long) {
  private[this] val ref = new NativePointer.Ref(this)
}

object NativePointer {

  import java.lang.ref.{PhantomReference, ReferenceQueue}

  private[NativePointer] class Ref(ptr: NativePointer) extends PhantomReference[NativePointer](ptr, queue) {
    val value = ptr.value
  }

  private[this] val queue = new ReferenceQueue[NativePointer]

  private[this] val thread = new Thread(() => while(true) {
    val ref = queue.remove().asInstanceOf[Ref]
    ref.clear()
    NativeLib.free(ref.value)
  })
  thread.setName("native-gc")
  thread.setDaemon(true)
  thread.setUncaughtExceptionHandler((_, e) => e.printStackTrace())
  thread.start()

}
