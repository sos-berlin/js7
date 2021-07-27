package js7.tests

import java.nio.file.Files.move
import java.nio.file.Paths
import js7.agent.RunningAgent
import js7.agent.data.Problems.{AgentAlreadyCreatedProblem, AgentNotCreatedProblem, AgentRunIdMismatchProblem}
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.io.file.FileUtils.{copyDirectory, deleteDirectoryContentRecursively, deleteDirectoryRecursively}
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
import js7.data.item.BasicItemEvent.ItemDeleted
import js7.data.item.ItemOperation.{AddOrChangeSigned, AddOrChangeSimple, AddVersion, DeleteSimple, RemoveVersioned}
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
    controllerApi.stop.await(99.s)
    controller.terminate() await 99.s
    super.afterAll()
  }

  "Add AgentRef and run an order" in {
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
    controller.runOrder(FreshOrder(OrderId("🔵"), workflow.path, deleteWhenTerminated = true))
  }

  private lazy val outdatedState = agentFileTree.stateDir.resolveSibling(Paths.get("state~"))

  "Delete AgentRef" in {
    copyDirectory(agentFileTree.stateDir, outdatedState)

    assert(controllerApi.updateItems(Observable(DeleteSimple(agentPath))).await(99.s) ==
      Left(ItemIsStillReferencedProblem(agentPath, workflow.path ~ v1)))

    val eventId = controller.eventWatch.lastAddedEventId

    controllerApi.updateItems(Observable(
      DeleteSimple(agentPath),
      AddVersion(VersionId("DELETE")),
      RemoveVersioned(workflow.path))
    ).await(99.s).orThrow

    controller.eventWatch.await[ItemDeleted](_.event.key == agentPath, after = eventId)
    agent.terminate() await 99.s
  }

  "Add AgentRef again but keep Agent's journal: should fail" in {
    agent = RunningAgent.startForTest(agentFileTree.agentConfiguration) await 99.s

    val eventId = controller.eventWatch.lastFileTornEventId
    val versionId = VersionId("AGAIN")
    val agentRef = AgentRef(agentPath, Uri(s"http://127.0.0.1:$agentPort1"))
    controllerApi
      .updateItems(
        Observable(
          AddOrChangeSimple(agentRef),
          AddVersion(versionId),
          AddOrChangeSigned(sign(workflow withVersion versionId).signedString)))
      .await(99.s).orThrow

    controller.eventWatch.await[AgentCouplingFailed](
      _.event.problem == AgentAlreadyCreatedProblem,
      after = eventId)
    agent.terminate().await(99.s)
  }

  "Restart Agent and do not keep its state" in {
    deleteDirectoryContentRecursively(agentFileTree.stateDir)
    agent = RunningAgent.startForTest(agentFileTree.agentConfiguration) await 99.s

    controller.runOrder(FreshOrder(OrderId("AGAIN"), workflow.path))
    agent.terminate() await 99.s
  }

  "Change Agent's URI and keep Agent's state (move the Agent)" in {
    val agentRef = AgentRef(agentPath, Uri(s"http://127.0.0.1:$agentPort2"))
    agent = RunningAgent.startForTest(
      agentFileTree.agentConfiguration.copy(
        webServerPorts = List(WebServerPort.localhost(agentPort2)))
    ) await 99.s
    controllerApi.updateUnsignedSimpleItems(Seq(agentRef)).await(99.s).orThrow
    controller.runOrder(FreshOrder(OrderId("🔶"), workflow.path))
    agent.terminate() await 99.s
  }

  "Use AgentRef with an outdated Agent: coupling fails" in {
    deleteDirectoryRecursively(agentFileTree.stateDir)
    move(outdatedState, agentFileTree.stateDir)
    val eventId = controller.eventWatch.lastFileTornEventId

    agent = RunningAgent.startForTest(
      agentFileTree.agentConfiguration.copy(
        webServerPorts = List(WebServerPort.localhost(agentPort2)))
    ) await 99.s

    controller.eventWatch.await[AgentCouplingFailed](
      _.event.problem == AgentRunIdMismatchProblem(agentPath),
      after = eventId)

    agent.terminate() await 99.s
  }

  "Change Agent's URI and start Agent with clean state: fails" in {
    val agentRef = AgentRef(agentPath, Uri(s"http://127.0.0.1:$agentPort3"))
    // DELETE AGENT'S STATE DIRECTORY
    deleteDirectoryContentRecursively(agentFileTree.stateDir)
    agent = RunningAgent.startForTest(
      agentFileTree.agentConfiguration.copy(
        webServerPorts = List(WebServerPort.localhost(agentPort3)))
    ) await 99.s

    val eventId = controller.eventWatch.lastFileTornEventId
    controllerApi.updateUnsignedSimpleItems(Seq(agentRef)).await(99.s).orThrow
    controller.eventWatch.await[AgentCouplingFailed](
      _.event.problem == AgentNotCreatedProblem,
      after = eventId)

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
