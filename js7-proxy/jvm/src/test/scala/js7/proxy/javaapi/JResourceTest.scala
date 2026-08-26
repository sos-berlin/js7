package js7.proxy.javaapi

import cats.effect.unsafe.IORuntime
import cats.effect.{IO, Resource}
import java.util.concurrent.CompletableFuture
import js7.base.test.OurAsyncTestSuite
import js7.base.utils.Atomic
import js7.base.utils.Atomic.extensions.:=
import reactor.core.publisher.Flux
import scala.jdk.CollectionConverters.*

final class JResourceTest extends OurAsyncTestSuite:

  private given IORuntime = ioRuntime

  "use" in:
    val acquired = Atomic(false)
    val released = Atomic(false)
    val jResource = JResource(Resource.make(
      acquire = IO:
        assert(!acquired.getAndSet(true))
        "TEST")(
      release = a => IO:
        assert(a == "TEST" && acquired.get && !released.getAndSet(true))))

    for
      result <- IO.fromCompletableFuture:
        IO:
          jResource.use: a =>
            assert(a == "TEST" && acquired.get && !released.get)
            CompletableFuture.completedFuture(s"$a-DONE")
      _ = assert(result == "TEST-DONE" && acquired.get && released.get)

      // Test release on failed future
      failedAcquired = Atomic(false)
      failedReleased = Atomic(false)
      failedJResource = JResource(Resource.make(
        acquire = IO:
          failedAcquired := true
          "FAIL-TEST")(
        release = _ => IO:
          failedReleased := true))
      testException = RuntimeException("TEST-ERROR")
      attemptResult <- IO.fromCompletableFuture:
        IO:
          failedJResource.use: a =>
            CompletableFuture.failedFuture[String](testException)
      .attempt
      _ = assert(attemptResult == Left(testException) && failedAcquired.get && failedReleased.get)

      // Test release on thrown exception in body
      thrownAcquired = Atomic(false)
      thrownReleased = Atomic(false)
      thrownJResource = JResource(Resource.make(
        acquire = IO:
          thrownAcquired := true
          "THROW-TEST")(
        release = _ => IO:
          thrownReleased := true))
      thrownException = RuntimeException("THROWN-ERROR")
      thrownAttemptResult <- IO.fromCompletableFuture:
        IO:
          thrownJResource.use: (a: String) =>
            throw thrownException
      .attempt
      _ =
        assert(thrownAttemptResult == Left(thrownException)
          && thrownAcquired.get && thrownReleased.get)
    yield succeed

  "allocated" in:
    val acquired = Atomic(false)
    val released = Atomic(false)
    val jResource = JResource(Resource.make(
      acquire = IO:
        assert(!acquired.getAndSet(true))
        "ALLOCATED-TEST")(
      release = a => IO:
        assert(a == "ALLOCATED-TEST" && acquired.get && !released.getAndSet(true))))

    for
      jAllocated <- IO.fromCompletableFuture(IO:
        jResource.allocate)
      _ <- IO:
        assert(jAllocated.allocatedThing == "ALLOCATED-TEST" && acquired.get && !released.get)
      _ <- IO.fromCompletableFuture(IO:
        jAllocated.release)
    yield
      assert(acquired.get && released.get)

  "asFlux" in:
    val acquired = Atomic(false)
    val released = Atomic(false)
    val jResource = JResource(Resource.make(
      acquire = IO:
        assert(!acquired.getAndSet(true))
        "TEST")(
      release = a => IO:
        assert(a == "TEST" && acquired.get && !released.getAndSet(true))))

    assert(!acquired.get && !released.get)
    val result =
      jResource.asFlux.flatMap: resource =>
        assert(acquired.get && !released.get)
        Flux.just(1, 2, 3)
      .toIterable.asScala.toList
    assert(acquired.get && released.get && result == List(1, 2, 3))
