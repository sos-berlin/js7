package js7.tests.controller.proxy

import js7.base.configutils.Configs.HoconStringInterpolator
import js7.data.agent.AgentPath
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.proxy.javaapi.JControllerApi
import js7.tests.controller.proxy.JFileWatchTest._
import js7.tests.jobs.DeleteFileJob
import js7.tests.testenv.ControllerAgentForScalaTest
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AnyFreeSpec

final class JFileWatchTest extends AnyFreeSpec with ControllerAgentForScalaTest
{
  protected def agentIds = Seq(agentId)
  protected def versionedItems = Seq(workflow)
  override protected def controllerConfig = config"""
    js7.web.server.auth.public = on
    """
  override protected def agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = on
    """
  private lazy val jControllerApi = new JControllerApi(controllerApi)

  "JFileWatch" in {
    JFileWatchTester.test(jControllerApi)
  }
}

object JFileWatchTest
{
  private val agentId = AgentPath("AGENT")
  private val workflow = Workflow.of(WorkflowPath("WORKFLOW"),
    DeleteFileJob.execute(agentId))
}
