package js7.tests

import js7.agent.RunningAgent
import js7.agent.data.Problems.AgentRunIdMismatchProblem
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.io.file.FileUtils.deleteDirectoryContentRecursively
import js7.base.problem.Checked._
import js7.base.thread.Futures.implicits._
import js7.base.thread.MonixBlocking.syntax._
import js7.base.time.ScalaTime._
import js7.base.web.Uri
import js7.common.akkahttp.web.data.WebServerPort
import js7.common.utils.FreeTcpPortFinder.findFreeTcpPorts
import js7.data.Problems.ItemIsStillReferencedProblem
import js7.data.agent.AgentRefStateEvent.AgentCouplingFailed
import js7.data.agent.{AgentPath, AgentRef}
import js7.data.item.BasicItemEvent.ItemDestroyed
import js7.data.item.ItemOperation.{AddOrChangeSigned, AddOrChangeSimple, AddVersion, DeleteSimple, DeleteVersioned}
import js7.data.item.VersionId
import js7.data.order.{FreshOrder, OrderId}
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.UpdateAgentRefsTest._
import js7.tests.jobs.EmptyJob
import js7.tests.testenv.ControllerTestUtils.newControllerApi
import js7.tests.testenv.{DirectoryProvider, DirectoryProviderForScalaTest}
import monix.execution.Scheduler.Implicits.global
import monix.reactive.Observable
import org.scalatest.freespec.AnyFreeSpec

final class UpdateAgentRefsTest extends AnyFreeSpec with DirectoryProviderForScalaTest
{
  override protected def controllerConfig = config"""
    js7.auth.users.TEST-USER.permissions = [ UpdateItem ]
    js7.controller.agent-driver.command-batch-delay = 0ms
    js7.controller.agent-driver.event-buffer-delay = 5ms
    """

  override protected def agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = on"""

  protected val agentPaths = Nil
  protected val items = Nil

  private lazy val agentPort1 :: agentPort2 :: agentPort3 :: Nil = findFreeTcpPorts(3)
  private lazy val agentFileTree = new DirectoryProvider.AgentTree(directoryProvider.directory,
    agentPath, "AGENT", agentPort1, config = agentConfig)
  private lazy val controller = directoryProvider.startController() await 99.s
  private lazy val controllerApi = newControllerApi(controller, Some(directoryProvider.controller.userAndPassword))
  private var agent: RunningAgent = null

  override def afterAll() = {
    controllerApi.close()
    controller.terminate() await 99.s
    super.afterAll()
  }

  "Standard operation" in {
    directoryProvider.prepareAgentFiles(agentFileTree)

    val agentRef = AgentRef(agentPath, Uri(s"http://127.0.0.1:$agentPort1"))
    agent = RunningAgent.startForTest(agentFileTree.agentConfiguration) await 99.s

    controllerApi
      .updateItems(
        Observable(
          AddOrChangeSimple(agentRef),
          AddVersion(v1),
          AddOrChangeSigned(sign(workflow withVersion v1).signedString)))
      .await(99.s).orThrow
    controller.runOrder(FreshOrder(OrderId("🔵"), workflow.path), remove=true)
  }

  "Delete AgentRef" in {
    assert(controllerApi.updateItems(Observable(DeleteSimple(agentPath))).await(99.s) ==
      Left(ItemIsStillReferencedProblem(agentPath, workflow.path ~ v1)))

    val eventId = controller.eventWatch.lastAddedEventId

    controllerApi.updateItems(Observable(
      DeleteSimple(agentPath),
      AddVersion(VersionId("DELETE")),
      DeleteVersioned(workflow.path))
    ).await(99.s).orThrow

    controller.eventWatch.await[ItemDestroyed](_.event.key == agentPath, after = eventId)
    agent.terminate() await 99.s
  }

  "Add AgentRef again" in {
    deleteDirectoryContentRecursively(agentFileTree.stateDir)
    agent = RunningAgent.startForTest(agentFileTree.agentConfiguration) await 99.s

    val versionId = VersionId("AGAIN")
    val agentRef = AgentRef(agentPath, Uri(s"http://127.0.0.1:$agentPort1"))
    controllerApi
      .updateItems(
        Observable(
          AddOrChangeSimple(agentRef),
          AddVersion(versionId),
          AddOrChangeSigned(sign(workflow withVersion versionId).signedString)))
      .await(99.s).orThrow
    controller.runOrder(FreshOrder(OrderId("AGAIN"), workflow.path))
  }

  "Change Agent's URI and keep Agent's state" in {
    agent.terminate() await 99.s
    val agentRef = AgentRef(agentPath, Uri(s"http://127.0.0.1:$agentPort2"))
    agent = RunningAgent.startForTest(
      agentFileTree.agentConfiguration.copy(
        webServerPorts = List(WebServerPort.localhost(agentPort2)))
    ) await 99.s
    controllerApi.updateUnsignedSimpleItems(Seq(agentRef)).await(99.s).orThrow
    controller.runOrder(FreshOrder(OrderId("🔶"), workflow.path))
  }

  "Change Agent's URI and start Agent with clean state: should fail" in {
    agent.terminate() await 99.s
    val agentRef = AgentRef(agentPath, Uri(s"http://127.0.0.1:$agentPort3"))
    // DELETE AGENT'S STATE DIRECTORY
    deleteDirectoryContentRecursively(agentFileTree.stateDir)
    agent = RunningAgent.startForTest(
      agentFileTree.agentConfiguration.copy(
        webServerPorts = List(WebServerPort.localhost(agentPort3)))
    ) await 99.s
    val beforeUpdate = controller.eventWatch.lastFileTornEventId
    controllerApi.updateUnsignedSimpleItems(Seq(agentRef)).await(99.s).orThrow
    controller.addOrderBlocking(FreshOrder(OrderId("❌"), workflow.path))
    controller.eventWatch.await[AgentCouplingFailed](
      _.event.problem == AgentRunIdMismatchProblem(agentPath),
      after = beforeUpdate)
    agent.terminate() await 99.s
  }
}

object UpdateAgentRefsTest
{
  private val agentPath = AgentPath("AGENT")
  private val v1 = VersionId("1")
  private val workflow = Workflow(WorkflowPath("WORKFLOW"), Vector(
    EmptyJob.execute(agentPath)))
}
