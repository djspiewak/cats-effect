package cats.effect.unsafe

abstract class PollingRuntime[+P <: Poller] {
  def buildPoller(reportFailure: Throwable => Unit): P
}
