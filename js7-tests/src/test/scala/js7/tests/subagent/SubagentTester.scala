package js7.tests.subagent

import cats.effect.Resource
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.log.Logger
import js7.base.thread.MonixBlocking.syntax._
import js7.base.time.ScalaTime._
import js7.base.utils.ScalaUtils.syntax._
import js7.base.web.Uri
import js7.common.utils.FreeTcpPortFinder.findFreeTcpPort
import js7.data.subagent.SubagentItemStateEvent.SubagentDedicated
import js7.data.subagent.{SubagentId, SubagentItem}
import js7.subagent.BareSubagent
import js7.tests.subagent.SubagentMultipleOrdersTest.agentPath
import js7.tests.subagent.SubagentTester._
import js7.tests.testenv.ControllerAgentForScalaTest
import monix.eval.Task
import monix.execution.Scheduler
import org.scalatest.Suite
import scala.util.control.NonFatal

trait SubagentTester extends ControllerAgentForScalaTest
{
  this: Suite =>

  override protected val controllerConfig = config"""
    js7.auth.users.TEST-USER.permissions = [ UpdateItem ]
    js7.journal.remove-obsolete-files = false
    js7.controller.agent-driver.command-batch-delay = 0ms
    js7.controller.agent-driver.event-buffer-delay = 10ms
    """.resolveWith(super.controllerConfig)

  override protected def agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = true
    js7.auth.subagents.BARE-SUBAGENT = "AGENT-PASSWORD"
    """.resolveWith(super.agentConfig)

  protected val bareSubagentId = SubagentId("BARE-SUBAGENT")
  protected lazy val bareSubagentItem = SubagentItem(
    bareSubagentId,
    agentPath,
    Uri(s"http://localhost:${findFreeTcpPort()}"))

  protected val scheduler: Scheduler

  implicit private def implicitScheduler = scheduler

  protected final def runSubagent[A](
    subagentItem: SubagentItem,
    suffix: String = "",
    awaitDedicated: Boolean = true,
    suppressSignatureKeys: Boolean = false)
    (body: BareSubagent => A)
  : A =
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
      .await(99.s)

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
