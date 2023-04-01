package js7.tests.jobs

import cats.effect.ExitCase
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.monixutils.MonixBase.syntax.*
import js7.base.utils.ScalaUtils.syntax.RichJavaClass
import js7.data.order.Outcome
import js7.launcher.OrderProcess
import js7.launcher.internal.InternalJob
import js7.tests.jobs.SemaphoreJob.*
import monix.catnap.Semaphore
import monix.eval.Task
import monix.execution.Scheduler
import scala.reflect.ClassTag

abstract class SemaphoreJob(companion: SemaphoreJob.Companion[? <: SemaphoreJob])
extends InternalJob
{
  final def toOrderProcess(step: Step) =
    OrderProcess {
      val orderId = step.order.id
      logger.debugTask(s"SemaphoreJob/$orderId")(
        step
          .outTaskObserver.send(companion.stdoutLine)
          .*>(companion.semaphore
            .flatMap(sema =>
              sema
                .tryAcquire
                .flatMap {
                  case true => Task.unit
                  case false =>
                    sema.count
                      .flatMap { n =>
                        logger.info(s"🔴 $orderId acquire ... (n=$n)")
                        sema.acquire
                      }
                      .guaranteeCase(exitCase => Task {
                        exitCase match {
                          case ExitCase.Error(_) => logger.debug(s"💥 $orderId $exitCase")
                          case ExitCase.Canceled => logger.debug(s"⚫️ $orderId $exitCase")
                          case ExitCase.Completed => logger.debug(s"🔵 $orderId acquired")
                        }
                      })
                })
            .logWhenItTakesLonger(s"${getClass.shortClassName}.semaphore.acquire/${step.order.id}")
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
