package js7.tests.controller.proxy

import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.test.OurTestSuite
import js7.data.agent.AgentPath
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.proxy.javaapi.JControllerApi
import js7.tests.controller.proxy.JFileWatchTest.*
import js7.tests.jobs.DeleteFileJob
import js7.tests.testenv.ControllerAgentForScalaTest
import monix.execution.Scheduler.Implicits.traced

final class JFileWatchTest extends OurTestSuite with ControllerAgentForScalaTest
{
  protected def agentPaths = Seq(agentPath)
  protected def items = Seq(workflow)
  override protected def controllerConfig = config"""
    js7.web.server.auth.public = on
    """
  override protected def agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = on
    """
  private lazy val jControllerApi = new JControllerApi(controller.api, config = controller.config)

  "JFileWatch" in {
    JFileWatchTester.testFileOrder(jControllerApi)
    JFileWatchTester.testFileWatchApi(jControllerApi)
  }
}

object JFileWatchTest
{
  private val agentPath = AgentPath("AGENT")
  private val workflow = Workflow.of(WorkflowPath("WORKFLOW"),
    DeleteFileJob.execute(agentPath))
}
