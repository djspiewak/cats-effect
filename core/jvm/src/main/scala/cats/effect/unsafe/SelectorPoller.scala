package cats.effect
package unsafe

import java.nio.channels.SelectableChannel
import java.nio.channels.spi.AbstractSelector

final class SelectorPoller(
    selector: AbstractSelector)(
    reportFailure: Throwable => Unit)
    extends Poller {

  import SelectorPoller.CallbackNode

  def close(): Unit =
    selector.close()

  def interrupt(targetThread: Thread): Unit = {
    selector.wakeup()
    ()
  }

  def poll(nanos: Long): Boolean = {
    val millis = if (nanos >= 0) nanos / 1000000 else -1

    if (millis == 0) selector.selectNow()
    else if (millis > 0) selector.select(millis)
    else selector.select()

    if (selector.isOpen()) { // closing selector interrupts select
      val ready = selector.selectedKeys().iterator()
      while (ready.hasNext()) {
        val key = ready.next()
        ready.remove()

        val readyOps = key.readyOps()

        var head: CallbackNode = null
        var prev: CallbackNode = null
        var node = key.attachment().asInstanceOf[CallbackNode]
        while (node ne null) {
          val next = node.next

          if ((node.interest & readyOps) != 0) { // execute callback and drop this node
            val cb = node.callback
            if (cb != null) cb(readyOps)
            if (prev ne null) prev.next = next
          } else { // keep this node
            prev = node
            if (head eq null)
              head = node
          }

          node = next
        }

        // reset interest in triggered ops
        key.interestOps(key.interestOps() & ~readyOps)
        key.attach(head)
      }

      !selector.keys().isEmpty()
    } else false
  }

  def select(ch: SelectableChannel, ops: Int)(cb: Int => Unit): () => Unit = {
    val key = ch.keyFor(selector)

    val node = if (key eq null) { // not yet registered on this selector
      val node = new CallbackNode(ops, cb, null)
      ch.register(selector, ops, node)
      node
    } else { // existing key
      // mixin the new interest
      key.interestOps(key.interestOps() | ops)
      val node =
        new CallbackNode(ops, cb, key.attachment().asInstanceOf[CallbackNode])
      key.attach(node)
      node
    }

    { () =>
      // set all interest bits
      node.interest = -1
      // clear for gc
      node.callback = null
    }
  }
}

private object SelectorPoller {
  private[SelectorPoller] final class CallbackNode(
      var interest: Int,
      var callback: Int => Unit,
      var next: CallbackNode)
}
