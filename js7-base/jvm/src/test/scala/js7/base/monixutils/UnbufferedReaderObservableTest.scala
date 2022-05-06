package js7.base.monixutils

import java.io.{PipedReader, PipedWriter}
import monix.eval.Task
import monix.execution.Scheduler.Implicits.traced
import monix.execution.schedulers.TestScheduler
import org.scalatest.freespec.AnyFreeSpec

final class UnbufferedReaderObservableTest extends AnyFreeSpec
{
  "UnbufferedReaderObservable" in {
    val scheduler = TestScheduler()
    val w = new PipedWriter
    val r = new PipedReader(w)

    val whenCompleted = UnbufferedReaderObservable(Task.pure(r))
      .toListL.runToFuture

    val expected = List("EINS", "ZWEI", "DREI")
    for (s <- expected) {
      w.write(s)
      w.flush()
      scheduler.tick()
    }
    w.close()

    for (list <- whenCompleted) yield {
      assert(list == expected)
    }
  }
}
