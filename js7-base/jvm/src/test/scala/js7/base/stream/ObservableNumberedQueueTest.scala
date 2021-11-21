package js7.base.stream

import js7.base.problem.{Checked, Problem}
import js7.base.stream.ObservableNumberedQueueTest._
import js7.base.thread.MonixBlocking.syntax._
import js7.base.time.ScalaTime._
import js7.base.time.WaitForCondition.waitForCondition
import js7.base.utils.ScalaUtils.syntax._
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AnyFreeSpec
import scala.collection.mutable

final class ObservableNumberedQueueTest extends AnyFreeSpec
{
  private val queue = new ObservableNumberedQueue[X]

  private def observe(after: Long, take: Int): Checked[List[Numbered[X]]] =
    queue
      .observable(after)
      .await(99.s)
      .map(_
        .take(take)
        .toListL
        .await(99.s))

  "Enqueue some values" in {
    queue.enqueue(Seq(X("a"), X("b"))).await(99.s)
    assert(observe(0, take = 2) == Right(List(Numbered(1, X("a")), Numbered(2, X("b")))))
    assert(observe(1, take = 1) == Right(List(Numbered(2, X("b")))))
    assert(observe(2, take = 0) == Right(Nil))
    assert(observe(3, take = 0) ==
      Left(Problem("Unknown Numbered[ObservableNumberedQueueTest::X]: #3")))

    queue.enqueue(Seq(X("c"))).await(99.s)
    assert(observe(0, take = 3) ==
      Right(List(
        Numbered(1, X("a")),
        Numbered(2, X("b")),
        Numbered(3, X("c")))))
  }

  "Enqueue more values after observation" in {
    val buffer = mutable.Buffer.empty[Numbered[X]]
    val future = queue
      .observable(0)
      .await(99.s).orThrow
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

    future.cancel()
  }

  "releaseUntil" in {
    queue.releaseUntil(0).await(99.s).orThrow
    assert(observe(0, take = 1) == Right(List(Numbered(1, X("a")))))

    queue.releaseUntil(1).await(99.s).orThrow
    assert(observe(0, take = 1) ==
      Left(Problem("Unknown Numbered[ObservableNumberedQueueTest::X]: #0")))

    assert(observe(1, take = 1) == Right(List(Numbered(2, X("b")))))

    assert(queue.releaseUntil(6).await(99.s) == Left(Problem("releaseUntil(6) > last.number ?")))
  }
}

object ObservableNumberedQueueTest
{
  private case class X(string: String)
}
