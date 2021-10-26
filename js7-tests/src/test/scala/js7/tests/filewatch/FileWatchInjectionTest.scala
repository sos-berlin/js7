package js7.tests.filewatch

import js7.base.configutils.Configs._
import js7.base.io.file.FileUtils.syntax._
import js7.base.problem.Checked._
import js7.base.thread.MonixBlocking.syntax._
import js7.base.time.ScalaTime._
import js7.data.agent.AgentPath
import js7.data.item.BasicItemEvent.ItemAttached
import js7.data.orderwatch.{FileWatch, OrderWatchPath}
import js7.data.value.expression.Expression.StringConstant
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.filewatch.FileWatchInjectionTest._
import js7.tests.testenv.ControllerAgentForScalaTest
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AnyFreeSpec
import scala.concurrent.TimeoutException

final class FileWatchInjectionTest extends AnyFreeSpec with ControllerAgentForScalaTest
{
  protected val agentPaths = Seq(agentPath)
  protected val items = Seq(workflow)
  override protected val controllerConfig = config"""
    js7.auth.users.TEST-USER.permissions = [ UpdateItem ]"""

  private val sourceDirectory = directoryProvider.agents(0).dataDir / "tmp/files"

  private lazy val fileWatch = FileWatch(
    OrderWatchPath("TEST-WATCH"),
    workflow.path,
    agentPath,
    StringConstant(sourceDirectory.toString))

  "Start with existing file" in {
    controllerApi.updateUnsignedSimpleItems(Seq(fileWatch)).await(99.s).orThrow
    // TODO SimpleItemAttachmentFailed
    intercept[TimeoutException] {
      eventWatch.await[ItemAttached](_.event.key == fileWatch.path, timeout = 1.s)
    }
  }
}

object FileWatchInjectionTest {
  private val agentPath = AgentPath("AGENT-A")
  private val workflow = Workflow(WorkflowPath("WORKFLOW"), Nil)
}
