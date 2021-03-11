package js7.tests.fileordersource

import js7.base.configutils.Configs._
import js7.base.io.file.FileUtils.syntax._
import js7.base.problem.Checked._
import js7.base.thread.MonixBlocking.syntax._
import js7.base.time.ScalaTime._
import js7.data.agent.AgentId
import js7.data.item.SimpleItemEvent.SimpleItemAttached
import js7.data.ordersource.{FileOrderSource, OrderSourceId}
import js7.data.workflow.WorkflowPath
import js7.tests.testenv.ControllerAgentForScalaTest
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AnyFreeSpec
import scala.concurrent.TimeoutException

final class FileOrderSourceInjectionTest extends AnyFreeSpec with ControllerAgentForScalaTest
{
  private val aAgentId = AgentId("AGENT-A")
  protected val agentIds = Seq(aAgentId)
  protected val versionedItems = Nil
  override protected val controllerConfig = config"""
    js7.web.server.auth.public = on"""

  private val sourceDirectory = directoryProvider.agents(0).dataDir / "tmp/files"

  private lazy val fileOrderSource = FileOrderSource(
    OrderSourceId("TEST-SOURCE"),
    WorkflowPath("WORKFLOW"),
    aAgentId,
    sourceDirectory.toString)

  "Start with existing file" in {
    controller.updateSimpleItemsAsSystemUser(Seq(fileOrderSource)).await(99.s).orThrow
    // TODO SimpleItemAttachmentFailed
    intercept[TimeoutException] {
      controller.eventWatch.await[SimpleItemAttached](_.event.id == fileOrderSource.id, timeout = 1.s)
    }
  }
}
