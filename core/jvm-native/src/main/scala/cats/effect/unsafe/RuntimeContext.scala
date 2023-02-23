package cats.effect.unsafe

import scala.concurrent.ExecutionContext

trait RuntimeContext[+P] extends ExecutionContext {
  def register(cb: P => Unit): Unit
}
