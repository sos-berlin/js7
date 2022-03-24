package js7.tests.subagent

import cats.effect.Resource
import js7.base.auth.Admission
import js7.base.log.Logger
import js7.base.thread.MonixBlocking.syntax.RichTask
import js7.base.time.ScalaTime._
import js7.base.utils.ScalaUtils.syntax._
import js7.controller.RunningController
import js7.controller.client.AkkaHttpControllerApi.admissionsToApiResources
import js7.data.subagent.SubagentItem
import js7.data.subagent.SubagentItemStateEvent.SubagentDedicated
import js7.proxy.ControllerApi
import js7.subagent.BareSubagent
import js7.tests.subagent.SubagentTester._
import js7.tests.testenv.DirectoryProviderForScalaTest
import monix.eval.Task
import monix.execution.Scheduler
import scala.util.control.NonFatal

trait SubagentTester
{
  this: DirectoryProviderForScalaTest =>

  protected val scheduler: Scheduler

  implicit private def implicitScheduler = scheduler

  protected lazy val controller: RunningController = directoryProvider
    .startController()
    .await(99.s)

  protected lazy val controllerApi = new ControllerApi(
    admissionsToApiResources(Seq(Admission(
      controller.localUri,
      Some(directoryProvider.controller.userAndPassword)
    )))(controller.actorSystem))

  import controller.eventWatch

  protected final def startSubagentTester() =
    controller

  protected final def stopSubagentTester() = {
    controllerApi.stop.await(99.s)
    controller.terminate().await(99.s)
  }

  protected final def runSubagent[A](
    subagentItem: SubagentItem,
    suffix: String = "",
    awaitDedicated: Boolean = true,
    suppressSignatureKeys: Boolean = false)
    (body: BareSubagent => A)
  : Task[A] =
    subagentResource(subagentItem,
      suffix = suffix,
      awaitDedicated = awaitDedicated,
      suppressSignatureKeys = suppressSignatureKeys
    ).use(subagent =>
      Task {
        try body(subagent)
        catch { case NonFatal(t) =>
          logger.error(t.toStringWithCauses, t.nullIfNoStackTrace)
          throw t
        }
      })

  protected final def subagentResource(
    subagentItem: SubagentItem,
    suffix: String = "",
    // FIXME awaitCoupled ?
    awaitDedicated: Boolean = true,
    suppressSignatureKeys: Boolean = false)
  : Resource[Task, BareSubagent] =
    Resource.suspend(Task {
      val eventId = eventWatch.lastAddedEventId
      directoryProvider
        .subagentResource(subagentItem, suffix = suffix, suppressSignatureKeys = suppressSignatureKeys)
        .map { subagent =>
          if (awaitDedicated) eventWatch.await[SubagentDedicated](after = eventId)
          subagent
      }
    })
}

object SubagentTester {
  private val logger = Logger(getClass)
}
