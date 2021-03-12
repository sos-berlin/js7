package js7.tests.controller.proxy

import js7.base.auth.Admission
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.controller.client.AkkaHttpControllerApi.admissionsToApiResources
import js7.data.agent.AgentId
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.proxy.ControllerApi
import js7.proxy.javaapi.JControllerApi
import js7.tests.controller.proxy.JFileOrderSourceTest._
import js7.tests.jobs.DeleteFileJob
import js7.tests.testenv.ControllerAgentForScalaTest
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AnyFreeSpec

final class JFileOrderSourceTest extends AnyFreeSpec with ControllerAgentForScalaTest
{
  protected def agentIds = Seq(agentId)
  protected def versionedItems = Seq(workflow)
  override protected def controllerConfig = config"""
    js7.web.server.auth.public = on
    """
  override protected def agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = on
    """
  private val admissions = Seq(Admission(controller.localUri))
  private lazy val controllerApi = new ControllerApi(admissionsToApiResources(admissions)(controller.actorSystem))
  private lazy val jControllerApi = new JControllerApi(controllerApi)

  override def afterAll() = {
    super.afterAll()
    controllerApi.close()
  }

  "JFileOrderSource" in {
    JFileOrderSourceTester.test(jControllerApi)
  }
}

object JFileOrderSourceTest
{
  private val agentId = AgentId("AGENT")
  private val workflow = Workflow.of(WorkflowPath("WORKFLOW"), DeleteFileJob.execute(agentId))
}
