package js7.tests.filewatch

import js7.base.configutils.Configs.*
import js7.base.io.file.FileUtils.syntax.*
import js7.base.io.file.FileUtils.touchFile
import js7.base.test.OurTestSuite
import js7.data.agent.AgentPath
import js7.data.order.OrderEvent.OrderFinished
import js7.data.order.OrderId
import js7.data.orderwatch.{FileWatch, OrderWatchPath}
import js7.data.value.expression.Expression.convenience.given
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.filewatch.FileWatch3Test.*
import js7.tests.jobs.DeleteFileJob
import js7.tests.testenv.ControllerAgentForScalaTest
import scala.language.implicitConversions

final class FileWatch3Test
extends OurTestSuite, ControllerAgentForScalaTest:

  protected val agentPaths = Seq(AgentPath("AGENT"))
  protected val items = Nil

  override protected val controllerConfig = config"""
    js7.auth.users.TEST-USER.permissions = [ UpdateItem ]
    """

  override protected def agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = on
    """

  "Change FileWatch when directory does not exists (bug JS-2196)" in :
    directoryProvider.withTemporaryDirectory: dir =>
      val workflow = Workflow(
        WorkflowPath("WORKFLOW"),
        Vector(DeleteFileJob.execute(agentPath)))

      withItem(workflow): workflow =>
        val fileWatch = FileWatch(
          OrderWatchPath("FILEWATCH"), workflow.path, agentPath,
          directoryExpr = (dir / "does-not-exist").toString)
        withItem(fileWatch, awaitDeletion = true): _ =>
          updateItem(fileWatch.copy(directoryExpr = dir.toString))
          touchFile(dir / "FILE")
          eventWatch.await[OrderFinished](_.key == OrderId(s"file:FILEWATCH:FILE"))


object FileWatch3Test:
  private val agentPath = AgentPath("AGENT")
