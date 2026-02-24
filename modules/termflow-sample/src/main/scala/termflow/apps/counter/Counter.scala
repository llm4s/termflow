package termflow.apps.counter

import scala.concurrent.{ ExecutionContext, Future }

final case class Counter(count: Int)

object Counter {

  extension (c: Counter) {
    def syncIncrement(): Counter = Counter(c.count + 1)
    def syncDecrement(): Counter = Counter(c.count - 1)

    def asyncIncrement()(using ec: ExecutionContext): Future[Counter] =
      Future {
        Thread.sleep(5000)
        Counter(c.count + 1)
      }

    def asyncDecrement()(using ec: ExecutionContext): Future[Counter] =
      Future {
        Thread.sleep(10000)
        Counter(c.count - 1)
      }
  }
}
