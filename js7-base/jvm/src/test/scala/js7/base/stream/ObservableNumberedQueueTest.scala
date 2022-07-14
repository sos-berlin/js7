package js7.base.stream

import js7.base.problem.{Problem, ProblemException}
import js7.base.stream.ObservableNumberedQueueTest.*
import js7.base.thread.MonixBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.time.WaitForCondition.waitForCondition
import js7.base.utils.ScalaUtils.syntax.*
import monix.eval.Task
import monix.execution.Scheduler.Implicits.traced
import monix.reactive.Observable
import org.scalatest.freespec.AnyFreeSpec
import scala.collection.mutable

final class ObservableNumberedQueueTest extends AnyFreeSpec
{
  private val queue = new ObservableNumberedQueue[X]

  private def observe(after: Long, take: Int): List[Numbered[X]] =
    queue
      .observable(after)
      .flatMap(Observable.fromIterable)
      .take(take)
      .toListL
      .await(99.s)

  "Enqueue some values" in {
    queue.enqueue(Seq(X("a"), X("b"))).await(99.s)
    assert(observe(0, take = 2) == List(Numbered(1, X("a")), Numbered(2, X("b"))))
    assert(observe(1, take = 1) == List(Numbered(2, X("b"))))
    assert(observe(2, take = 0) == Nil)

    // Too high after argument
    assert(observe(3, take = 0) == Nil)  // Invalid `after` is not detected due to take=0
    locally {
      val t = intercept[ProblemException](observe(3, take = 1))
      assert(t.problem == Problem(
        "Unknown number: Numbered[ObservableNumberedQueueTest::X]: #3 (must be >=0 and <=2)"))
    }
    locally {
      val t = intercept[ProblemException](observe(-1, take = 1))
      assert(t.problem == Problem(
        "Unknown number: Numbered[ObservableNumberedQueueTest::X]: #-1 (must be >=0 and <=2)"))
    }

    queue.enqueue(Seq(X("c"))).await(99.s)
    assert(observe(0, take = 3) ==
      List(
        Numbered(1, X("a")),
        Numbered(2, X("b")),
        Numbered(3, X("c"))))
  }

  "Enqueue more values after observation" in {
    val buffer = mutable.Buffer.empty[Numbered[X]]
    var isCompleted = false
    val future = queue
      .observable(0)
      .flatMap(Observable.fromIterable)
      .guarantee(Task { isCompleted = true })
      .foreach {
        buffer += _
      }

    waitForCondition(10.s, 10.ms)(buffer.length == 3)
    assert(buffer == Seq(
      Numbered(1, X("a")),
      Numbered(2, X("b")),
      Numbered(3, X("c"))))

    queue.enqueue(Seq(X("d"))).await(99.s)
    waitForCondition(10.s, 10.ms)(buffer.length == 4)
    assert(buffer(3) == Numbered(4, X("d")))

    queue.enqueue(Seq(X("e"))).await(99.s)
    waitForCondition(10.s, 10.ms)(buffer.length == 5)
    assert(buffer(4) == Numbered(5, X("e")))

    queue.enqueue(Seq(X("f"))).await(99.s)
    waitForCondition(10.s, 10.ms)(buffer.length == 6)
    assert(buffer(5) == Numbered(6, X("f")))

    future.cancel()
    waitForCondition(10.s, 10.ms)(isCompleted)
    assert(isCompleted)
  }

  "release" in {
    queue.release(0).await(99.s).orThrow
    assert(observe(0, take = 1) == List(Numbered(1, X("a"))))

    locally {
      queue.release(1).await(99.s).orThrow
      val t = intercept[ProblemException](observe(0, take = 1))
      assert(t.problem == Problem(
        "Unknown number: Numbered[ObservableNumberedQueueTest::X]: #0 (must be >=1 and <=6)"))
    }

    assert(observe(1, take = 1) == List(Numbered(2, X("b"))))

    locally {
      queue.release(3).await(99.s).orThrow
      val t = intercept[ProblemException](observe(0, take = 1))
      assert(t.problem == Problem(
        "Unknown number: Numbered[ObservableNumberedQueueTest::X]: #0 (must be >=3 and <=6)"))
    }

    assert(queue.release(7).await(99.s) == Left(Problem(
      "Unknown number: Numbered[ObservableNumberedQueueTest::X]: #7 (must be >=3 and <=6)")))
  }
}

object ObservableNumberedQueueTest
{
  private case class X(string: String)
}
