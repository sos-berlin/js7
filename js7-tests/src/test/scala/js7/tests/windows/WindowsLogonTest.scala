package js7.tests.windows

import java.util.Locale
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.io.process.KeyLogin
import js7.base.log.Logger
import js7.base.system.OperatingSystem.isWindows
import js7.base.utils.Collections.implicits._
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.data.agent.AgentPath
import js7.data.job.ScriptExecutable
import js7.data.order.OrderEvent.{OrderFinished, OrderProcessed, OrderStderrWritten, OrderStdoutWritten}
import js7.data.order.{FreshOrder, OrderId, Outcome}
import js7.data.workflow.instructions.Execute
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.executor.forwindows.{WindowsProcess, WindowsProcessCredentials}
import js7.tests.testenv.ControllerAgentForScalaTest
import js7.tests.windows.WindowsLogonTest._
import org.scalatest.freespec.AnyFreeSpec

final class WindowsLogonTest extends AnyFreeSpec with ControllerAgentForScalaTest
{
  private lazy val targetKey = sys.props.get(WindowsProcess.TargetSystemProperty).filter(_.nonEmpty)
  private lazy val workflow = Workflow(WorkflowPath("WORKFLOW") ~ "INITIAL",
    Vector(
      Execute(WorkflowJob(agentPath,
        ScriptExecutable(
          """echo WindowsLogonTest
            |set
            |""".stripMargin,
          login = targetKey.map(KeyLogin(_, withUserProfile = false)))))))

  protected def agentPaths = Seq(agentPath)
  protected def versionedItems = Seq(workflow)

  override protected def agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = on
    """

  if (isWindows) {
    "Windows Login" in {
      val events = controller.runOrder(FreshOrder(OrderId("WindowsLogonTest"), workflow.path))
        .map(_.value)
      val stdout = events.collect { case OrderStdoutWritten(chunk) => chunk }.fold_
      val stderr = events.collect { case OrderStderrWritten(chunk) => chunk }.fold_
      logger.info(s"stdout:\n$stdout")
      logger.info(s"stderr:\n$stderr")
      assert(events.collect { case o: OrderProcessed => o } == Seq(OrderProcessed(Outcome.succeededRC0)))
      assert(events.last == OrderFinished)

      val userName = targetKey.map(key => WindowsProcessCredentials.keyToUser(key).orThrow.string)
        .getOrElse(sys.env("USERNAME"))
        .toLowerCase(Locale.ROOT)
      assert(stdout.toLowerCase(Locale.ROOT).contains(s"username=$userName\r\n"))
    }
  }
}

object WindowsLogonTest
{
  private val agentPath = AgentPath("AGENT")
  private val logger = Logger[this.type]
}