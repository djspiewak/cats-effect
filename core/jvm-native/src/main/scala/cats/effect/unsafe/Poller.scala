package cats.effect.unsafe

trait Poller {
  def poll(limitNanos: Long): Boolean
  def interrupt(targetThread: Thread): Unit
  def close(): Unit
}
