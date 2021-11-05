package js7.subagent.client

import scala.collection.mutable

final class ObservableNumberedQueueTest extends AnyFreeSpec
{
  private val queue = new ObservableNumberedQueue[X]

  private def observe(after: Long, take: Int): Checked[List[Numbered[X]]] =
    queue
      .observable(after)
      .map(_
        .take(take)
        .toListL
        .await(99.s))

  "Enqueue some values" in {
    queue.enqueue(X("a"))
    queue.enqueue(X("b"))
    assert(observe(0, take = 2) == Right(List(Numbered(1, X("a")), Numbered(2, X("b")))))
    assert(observe(1, take = 1) == Right(List(Numbered(2, X("b")))))
    assert(observe(2, take = 0) == Right(Nil))
    assert(observe(3, take = 0) ==
      Left(Problem("Unknown Numbered[ObservableNumberedQueueTest::X]: #3")))

    queue.enqueue(X("c"))
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
      .orThrow
      .foreach {
        buffer += _
      }

    waitForCondition(10.s, 10.ms)(buffer.length == 3)
    assert(buffer == Seq(
      Numbered(1, X("a")),
      Numbered(2, X("b")),
      Numbered(3, X("c"))))

    queue.enqueue(X("d"))
    waitForCondition(10.s, 10.ms)(buffer.length == 4)
    assert(buffer(3) == Numbered(4, X("d")))

    queue.enqueue(X("e"))
    waitForCondition(10.s, 10.ms)(buffer.length == 5)
    assert(buffer(4) == Numbered(5, X("e")))

    future.cancel()
  }

  "releaseUntil" in {
    queue.releaseUntil(0).orThrow
    assert(observe(0, take = 1) == Right(List(Numbered(1, X("a")))))

    queue.releaseUntil(1).orThrow
    assert(observe(0, take = 1) ==
      Left(Problem("Unknown Numbered[ObservableNumberedQueueTest::X]: #0")))

    assert(observe(1, take = 1) == Right(List(Numbered(2, X("b")))))

    assert(queue.releaseUntil(6) == Left(Problem("releaseUntil(6) > last.number ?")))
  }
}

object ObservableNumberedQueueTest
{
  private case class X(string: String)
}
