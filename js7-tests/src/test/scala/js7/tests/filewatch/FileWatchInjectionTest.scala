package js7.tests.filewatch

import js7.base.configutils.Configs.*
import js7.base.io.file.FileUtils.syntax.*
import js7.base.problem.Checked.*
import js7.base.test.OurTestSuite
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.data.agent.AgentPath
import js7.data.item.BasicItemEvent.ItemAttached
import js7.data.orderwatch.{FileWatch, OrderWatchPath}
import js7.data.value.expression.Expression.StringConstant
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.filewatch.FileWatchInjectionTest.*
import js7.tests.testenv.ControllerAgentForScalaTest
import scala.concurrent.TimeoutException

final class FileWatchInjectionTest extends OurTestSuite, ControllerAgentForScalaTest:

  protected val agentPaths = Seq(agentPath)
  protected val items = Seq(workflow)
  override protected val controllerConfig = config"""
    js7.auth.users.TEST-USER.permissions = [ UpdateItem ]"""

  private val sourceDirectory = directoryProvider.agentEnvs(0).dataDir / "tmp/files"

  private lazy val fileWatch = FileWatch(
    OrderWatchPath("TEST-WATCH"),
    workflow.path,
    agentPath,
    StringConstant(sourceDirectory.toString))

  "Start with existing file" in:
    controller.api.updateUnsignedSimpleItems(Seq(fileWatch)).await(99.s).orThrow
    // TODO SimpleItemAttachmentFailed
    intercept[TimeoutException]:
      eventWatch.await[ItemAttached](_.event.key == fileWatch.path, timeout = 1.s)


object FileWatchInjectionTest:
  private val agentPath = AgentPath("AGENT-A")
  private val workflow = Workflow(WorkflowPath("WORKFLOW"), Nil)
