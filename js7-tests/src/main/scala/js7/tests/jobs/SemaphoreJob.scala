package js7.tests.jobs

import cats.effect.ExitCase
import js7.base.log.Logger
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.order.Outcome
import js7.launcher.OrderProcess
import js7.launcher.internal.InternalJob
import js7.tests.jobs.SemaphoreJob.*
import monix.catnap.Semaphore
import monix.eval.Task
import monix.execution.Scheduler
import scala.concurrent.TimeoutException
import scala.concurrent.duration.Deadline.now
import scala.reflect.ClassTag

abstract class SemaphoreJob(companion: SemaphoreJob.Companion[? <: SemaphoreJob])
extends InternalJob
{
  final def toOrderProcess(step: Step) =
    OrderProcess {
      val orderId = step.order.id
      val semaName = s"${getClass.shortClassName}($orderId) semaphore"
      /*logger.debugTask(s"${getClass.shortClassName}($orderId)")*/(
        step
          .outTaskObserver.send(companion.stdoutLine)
          .*>(companion.semaphore
            .flatMap(sema =>
              sema
                .tryAcquire
                .flatMap {
                  case true => Task.unit
                  case false =>
                    val since = now
                    sema.count
                      .flatMap { count =>
                        logger.info(s"🟡 $semaName is locked (count=$count)")
                        val durations = Iterator(3.s, 7.s) ++ Iterator.continually(10.s)
                        Task
                          .defer(sema
                            .acquire
                            .timeoutTo(durations.next(), Task.raiseError(new TimeoutException)))
                          .onErrorRestartLoop(()) {
                            case (_: TimeoutException, _, retry) =>
                              sema.count.flatMap { count =>
                                logger.info(
                                  s"🟠 $semaName is still locked (count=$count) since ${since.elapsed.pretty}")
                                retry(())
                              }
                            case (t, _, _) => Task.raiseError(t)
                          }
                      }
                      .guaranteeCase(exitCase => Task(
                        exitCase match {
                          case ExitCase.Error(_)  => logger.info(s"💥 $semaName $exitCase")
                          case ExitCase.Canceled  => logger.info(s"⚫️ $semaName $exitCase")
                          case ExitCase.Completed => logger.info(s"🟢 $semaName acquired")
                        }))
                })
            .as(Outcome.succeeded)))
    }
}

object SemaphoreJob
{
  private val logger = Logger[this.type]

  abstract class Companion[I <: SemaphoreJob](implicit classTag: ClassTag[I])
  extends InternalJob.Companion[I]
  {
    val semaphore = Semaphore[Task](0).memoize
    private val name = classTag.runtimeClass.shortClassName
    val stdoutLine = getClass.simpleScalaName + "\n"

    def reset()(implicit s: Scheduler): Unit = {
      logger.debug(s"$name.reset")
      (for {
        sema <- semaphore
        count <- sema.count
        _ <-
          if (count > 0) sema.acquireN(count)
          else if (count < 0) sema.releaseN(-count)
          else Task.pure(sema)
      } yield ())
        .runSyncUnsafe()
    }

    def continue(n: Int = 1)(implicit s: Scheduler): Unit = {
      logger.debug(s"$name.continue($n)")
      semaphore
        .flatMap(_.releaseN(n))
        .runSyncUnsafe()
    }
  }
}
