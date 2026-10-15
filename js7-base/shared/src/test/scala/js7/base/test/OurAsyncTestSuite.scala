package js7.base.test

import cats.effect.syntax.temporal.*
import cats.effect.{GenTemporal, Sync}
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import js7.base.utils.ScalaUtils.syntax.RichJavaClass
import org.scalatest.Assertion
import org.scalatest.freespec.AsyncFreeSpec
import java.util.concurrent.Executors
import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.FiniteDuration

abstract class OurAsyncTestSuite
extends AsyncFreeSpec, LoggingAsyncFreeSpec, TestMixin, TestCatsEffect:

  /** ScalaTest starts the next test in a Future callback on this ExecutionContext.
    *
    * It must not be the IORuntime's compute pool, because LoggingAsyncFreeSpec blocks with
    * unsafeRunSync, which converts the compute thread into a blocker thread.
    * Probably this triggers cats-effect 3.7.1 issue #4674, letting the IORuntime hang forever. */
  override implicit def executionContext: ExecutionContext =
    OurAsyncTestSuite.executionContext

  protected final def testTimeout[F[_]](timeout: FiniteDuration)(test: F[Assertion])
    (using F: GenTemporal[F, Throwable])
  : F[Assertion] =
    test.timeout(timeout)

  protected final def repeatTest[F[_]](n: Int)(body: Int => F[Assertion])(using F: Sync[F])
  : F[Assertion] =
    (1).tailRecM: i =>
      F.delay(Logger(getClass.scalaName).info(s"╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌ repeatTest #$i"))
        >> body(i)
        .map: result =>
          if i < n then
            Left(i + 1)
          else
            Right(result)


object OurAsyncTestSuite:
  private val threadCounter = AtomicInteger(0)

  private lazy val executionContext: ExecutionContext =
    ExecutionContext.fromExecutorService:
      Executors.newCachedThreadPool: runnable =>
        val thread = Thread(runnable, s"ScalaTest-async-${threadCounter.incrementAndGet()}")
        thread.setDaemon(true)
        thread
