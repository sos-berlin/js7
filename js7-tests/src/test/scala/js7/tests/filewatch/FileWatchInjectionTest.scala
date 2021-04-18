package js7.tests.filewatch

import js7.base.configutils.Configs._
import js7.base.io.file.FileUtils.syntax._
import js7.base.problem.Checked._
import js7.base.thread.MonixBlocking.syntax._
import js7.base.time.ScalaTime._
import js7.data.agent.AgentId
import js7.data.item.CommonItemEvent.ItemAttached
import js7.data.orderwatch.{FileWatch, OrderWatchId}
import js7.data.workflow.WorkflowPath
import js7.tests.testenv.ControllerAgentForScalaTest
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AnyFreeSpec
import scala.concurrent.TimeoutException

final class FileWatchInjectionTest extends AnyFreeSpec with ControllerAgentForScalaTest
{
  private val aAgentId = AgentId("AGENT-A")
  protected val agentIds = Seq(aAgentId)
  protected val versionedItems = Nil
  override protected val controllerConfig = config"""
    js7.auth.users.TEST-USER.permissions = [ UpdateItem ]"""

  private val sourceDirectory = directoryProvider.agents(0).dataDir / "tmp/files"

  private lazy val fileWatch = FileWatch(
    OrderWatchId("TEST-WATCH"),
    WorkflowPath("WORKFLOW"),
    aAgentId,
    sourceDirectory.toString)

  "Start with existing file" in {
    controllerApi.updateUnsignedSimpleItems(Seq(fileWatch)).await(99.s).orThrow
    // TODO SimpleItemAttachmentFailed
    intercept[TimeoutException] {
      controller.eventWatch.await[ItemAttached](_.event.id == fileWatch.id, timeout = 1.s)
    }
  }
}
