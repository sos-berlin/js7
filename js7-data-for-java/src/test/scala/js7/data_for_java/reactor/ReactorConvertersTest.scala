package js7.data_for_java.reactor

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import fs2.Stream
import js7.base.fs2utils.StreamExtensions.+:
import js7.base.log.Logger
import js7.base.test.OurAsyncTestSuite
import js7.base.thread.CatsBlocking.syntax.await
import js7.base.time.ScalaTime
import js7.base.time.ScalaTime.*
import js7.base.utils.Atomic
import js7.base.utils.Atomic.extensions.*
import js7.data_for_java.reactor.ReactorConverters.{asFlux, asFs2Stream}
import js7.data_for_java.reactor.ReactorConvertersTest.*
import js7.tester.ScalaTestUtils
import js7.tester.ScalaTestUtils.awaitAndAssert
import org.scalatest.Assertions.assert
import reactor.core.publisher.Flux
import scala.jdk.CollectionConverters.*

final class ReactorConvertersTest extends OurAsyncTestSuite:

  private given IORuntime = ioRuntime

  "asFlux" - {
    "Blue sky case with resource acquisition" in:
      val preparation = Preparation()
      import preparation.{acquired, released}
      val flux = preparation.stream
        .evalTap(i => IO:
          assert(acquired.get == 1 && released.get == 0)
          logger.info(s"--> $i"))
        .asFlux
      assert(acquired.get == 0 && released.get == 0)

      val result = flux.toIterable(1).asScala.toSeq // Blocks !!!
      assert(result == Seq(1, 2, 3))
      assert(acquired.get == 1 && released.get == 1)

    "Failing Stream" in:
      val preparation = Preparation()
      import preparation.{acquired, last, released}
      val flux = preparation.stream
        .flatMap(_ +: Stream.raiseError[IO](new TestException))
        .asFlux

      intercept[TestException]:
        flux.toIterable(1).asScala.toSeq
      assert(acquired.get == 1 && released.get == 1 && last.get == 1/*not 2 ???*/)

    "Canceled Stream" in:
      val preparation = new Preparation
      import preparation.{acquired, last, released}
      val flux = preparation
        .stream
        .flatMap(_ +: Stream.never[IO])
        .asFlux

      val disposable = flux.subscribe()
      sleep(100.ms)
      assert(acquired.get == 1 && released.get == 0 && last.get == 1)
      disposable.dispose()
      // dispose() cancels in background. So we wait for the outcome:
      awaitAndAssert(acquired.get == 1 && released.get == 1 && last.get == 1)
  }

  "asFs2Stream" in {
    assert(Flux.just(1, 2, 3).asFs2Stream().compile.toList.await(9.s) == List(1, 2, 3))
    assert(Flux.just(1, 2, 3).asFs2Stream(bufferSize = 9).compile.toList.await(9.s) == List(1, 2, 3))
    assert(Flux.just(1, 2, 3).asFs2Stream().asFlux.toIterable().asScala.toList == List(1, 2, 3))
  }


object ReactorConvertersTest:

  private val logger = Logger[this.type]
  private final class TestException extends RuntimeException

  private final class Preparation:
    val acquired, released, last = Atomic(0)

    val stream: Stream[IO, Int] = Stream
      .bracket(
        acquire = IO:
          acquired += 1)(
        release = _ => IO:
          released += 1)
      .flatMap: i =>
        assert(i == 1 && acquired.get == 1 && released.get == 0)
        Stream(i, 2, 3)
      .evalTap(i => IO:
        last := i)
