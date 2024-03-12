package js7.tests.jobs

import cats.effect.{IO, Outcome}
import cats.effect.std.Semaphore
import cats.effect.unsafe.IORuntime
import js7.base.catsutils.UnsafeMemoizable.unsafeMemoize
import js7.base.log.Logger
import js7.base.monixlike.MonixLikeExtensions.onErrorRestartLoop
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.order.OrderOutcome
import js7.launcher.OrderProcess
import js7.launcher.internal.InternalJob
import js7.tests.jobs.SemaphoreJob.*
import scala.concurrent.TimeoutException
import scala.concurrent.duration.Deadline.now
import scala.reflect.ClassTag

abstract class SemaphoreJob(companion: SemaphoreJob.Companion[? <: SemaphoreJob])
extends InternalJob:

  final def toOrderProcess(step: Step) =
    val orderId = step.order.id
    val semaName = s"${getClass.shortClassName}($orderId) semaphore"
    OrderProcess(
      for
        _ <- step.writeOut(companion.stdoutLine)
        sema <- companion.semaphore
        acquired <- sema.tryAcquire
        count <- sema.count
        outcome <-
          if acquired then
            onAcquired(step, semaName)
          else
            untilAcquired(sema, semaName, count).as(OrderOutcome.succeeded)
      yield outcome)

  protected def onAcquired(step: Step, semaphoreName: String): IO[OrderOutcome.Completed] =
    IO:
      logger.info(s"⚪️ $semaphoreName acquired")
      OrderOutcome.succeeded

  private def untilAcquired(sema: Semaphore[IO], semaName: String, count: Long): IO[Unit] =
    IO.defer:
      val since = now
      logger.info(s"🟡 $semaName is locked (count=$count)")
      val durations = Iterator(3.s, 7.s) ++ Iterator.continually(10.s)
      IO
        .defer(sema
          .acquire
          .timeoutTo(durations.next(), IO.raiseError(new TimeoutException)))
        .onErrorRestartLoop(()):
          case (_: TimeoutException, _, retry) =>
            sema.count.flatMap { count =>
              logger.info(
                s"🟠 $semaName is still locked (count=$count) since ${since.elapsed.pretty}")
              retry(())
            }
          case (t, _, _) => IO.raiseError(t)
        .guaranteeCase(outcome => IO(outcome match
          case Outcome.Errored(t) => logger.error(s"💥 $semaName $outcome => ${t.toStringWithCauses}")
          case Outcome.Canceled() => logger.info(s"⚫️ $semaName $outcome")
          case Outcome.Succeeded(_) => logger.info(s"🟢 $semaName acquired")))


object SemaphoreJob:
  private val logger = Logger[this.type]

  abstract class Companion[I <: SemaphoreJob](implicit classTag: ClassTag[I])
  extends InternalJob.Companion[I]:
    val semaphore = Semaphore[IO](0).unsafeMemoize
    private val name = classTag.runtimeClass.shortClassName
    val stdoutLine = getClass.simpleScalaName + "\n"

    def reset()(using IORuntime): Unit =
      logger.debug(s"$name.reset")
      (for
        sema <- semaphore
        count <- sema.count
        _ <-
          if count > 0 then sema.acquireN(count)
          else if count < 0 then sema.releaseN(-count)
          else IO.pure(sema)
      yield ())
        .unsafeRunSync()

    def continue(n: Int = 1)(using IORuntime): Unit =
      val count = semaphore.flatMap(_.count).unsafeRunSync()
      logger.debug(s"$name.continue($n) count=$count")
      semaphore
        .flatMap(_.releaseN(n))
        .unsafeRunSync()
