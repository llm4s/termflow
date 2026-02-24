package termflow.apps.counter

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

opaque type Counter = Int

object Counter {
  def apply(count: Int): Counter = count

  extension (c: Counter) {
    def count: Int = c

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
