package apg


import java.util.concurrent.atomic.AtomicLong

import scala.annotation.tailrec

final class NativePointer(val value: Long) {
  NativePointer.register(this)
}

object NativePointer {

  import scala.ref.{PhantomReference, ReferenceQueue}
  import java.util.concurrent.atomic.AtomicReference

  private[this] val queue = new ReferenceQueue[NativePointer]
  private[this] val pointers = new AtomicReference(Set[Ref]())

  def register(ptr: NativePointer): Unit = register(new Ref(ptr))

  @tailrec
  private[this] def register(ref: Ref): Unit = {
    val current = pointers.get()
    if (!pointers.compareAndSet(current, current + ref))
      register(ref)
  }

  @tailrec
  private[this] def unregister(ref: Ref): Unit = {
    val current = pointers.get()
    if (!pointers.compareAndSet(current, current - ref))
      unregister(ref)
  }

  private[NativePointer] class Ref(ptr: NativePointer) extends PhantomReference[NativePointer](ptr, queue) {
    val value = ptr.value
  }

  private[this] val thread = new Thread(() => while(true) {
    val ref = queue.remove match {
      case Some(r: Ref) =>
        unregister(r)
        r.clear()
        NativeLib.free(r.value)
      case None =>
    }
  })
  thread.setName("native-gc")
  thread.setDaemon(true)
  thread.setUncaughtExceptionHandler((_, e) => e.printStackTrace())
  thread.start()

}
