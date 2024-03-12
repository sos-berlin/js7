package js7.base.stream

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import js7.base.monixlike.MonixLikeExtensions.unsafeToCancelableFuture
import js7.base.problem.{Problem, ProblemException}
import js7.base.stream.StreamNumberedQueueTest.*
import js7.base.test.OurAsyncTestSuite
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.thread.Futures.implicits.*
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.*
import js7.tester.ScalaTestUtils.awaitAndAssert
import scala.collection.mutable

final class StreamNumberedQueueTest extends OurAsyncTestSuite:

  private given IORuntime = ioRuntime

  private val streamNumberedQueue = new StreamNumberedQueue[X]

  private def observe(after: Long, take: Int): List[Numbered[X]] =
    streamNumberedQueue
      .stream(after)
      .take(take)
      .compile
      .toList
      .await(99.s)

  "Enqueue some values" in:
    streamNumberedQueue.enqueue(Seq(X("a"), X("b")))
      .await(99.s)

    assert(observe(0, take = 2) == List(Numbered(1, X("a")), Numbered(2, X("b"))))
    assert(observe(1, take = 1) == List(Numbered(2, X("b"))))
    assert(observe(2, take = 0) == Nil)

    // Too high after argument
    assert(observe(3, take = 0) == Nil)  // Invalid `after` is not detected due to take=0
    locally:
      val t = intercept[ProblemException](observe(3, take = 1))
      assert(t.problem == Problem(
        "Unknown number: Numbered[StreamNumberedQueueTest::X]: #3 (must be >=0 and <=2)"))
    locally:
      val t = intercept[ProblemException](observe(-1, take = 1))
      assert(t.problem == Problem(
        "Unknown number: Numbered[StreamNumberedQueueTest::X]: #-1 (must be >=0 and <=2)"))

    streamNumberedQueue.enqueue(Seq(X("c"))).await(99.s)
    assert(observe(0, take = 3) == List(
      Numbered(1, X("a")),
      Numbered(2, X("b")),
      Numbered(3, X("c"))))

  "Enqueue more values after observation" in:
    val buffer = mutable.Buffer.empty[Numbered[X]]
    var isCompleted = false
    val future = streamNumberedQueue
      .stream(0)
      .onFinalize(IO { isCompleted = true })
      .foreach(o => IO:
        buffer += o)
      .compile
      .drain
      .unsafeToCancelableFuture()

    awaitAndAssert(buffer.length == 3)
    assert(buffer == Seq(
      Numbered(1, X("a")),
      Numbered(2, X("b")),
      Numbered(3, X("c"))))

    streamNumberedQueue.enqueue(Seq(X("d"))).await(99.s)
    awaitAndAssert(buffer.length == 4)
    assert(buffer(3) == Numbered(4, X("d")))

    streamNumberedQueue.enqueue(Seq(X("e"))).await(99.s)
    awaitAndAssert(buffer.length == 5)
    assert(buffer(4) == Numbered(5, X("e")))

    streamNumberedQueue.enqueue(Seq(X("f"))).await(99.s)
    awaitAndAssert(buffer.length == 6)
    assert(buffer(5) == Numbered(6, X("f")))

    future.cancelToFuture().await(99.s)
    awaitAndAssert(isCompleted)
    assert(isCompleted)

  "release" in:
    streamNumberedQueue.release(0).await(99.s).orThrow
    assert(observe(0, take = 1) == List(Numbered(1, X("a"))))

    locally:
      streamNumberedQueue.release(1).await(99.s).orThrow
      val t = intercept[ProblemException](observe(0, take = 1))
      assert(t.problem == Problem(
        "Unknown number: Numbered[StreamNumberedQueueTest::X]: #0 (must be >=1 and <=6)"))

    assert(observe(1, take = 1) == List(Numbered(2, X("b"))))

    locally:
      streamNumberedQueue.release(3).await(99.s).orThrow
      val t = intercept[ProblemException](observe(0, take = 1))
      assert(t.problem == Problem(
        "Unknown number: Numbered[StreamNumberedQueueTest::X]: #0 (must be >=3 and <=6)"))

    assert(streamNumberedQueue.release(7).await(99.s) == Left(Problem(
      "Unknown number: Numbered[StreamNumberedQueueTest::X]: #7 (must be >=3 and <=6)")))


object StreamNumberedQueueTest:
  private case class X(string: String)
