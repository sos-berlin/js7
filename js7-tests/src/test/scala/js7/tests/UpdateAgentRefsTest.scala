package js7.tests

import cats.effect.unsafe.IORuntime
import fs2.Stream
import java.nio.file.Files.move
import java.nio.file.Paths
import js7.agent.TestAgent
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.io.file.FileUtils.{copyDirectoryContent, deleteDirectoryContentRecursively, deleteDirectoryRecursively}
import js7.base.problem.Checked.*
import js7.base.problem.Problem
import js7.base.test.OurTestSuite
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.utils.AllocatedForJvm.*
import js7.base.web.Uri
import js7.common.pekkohttp.web.data.WebServerPort
import js7.common.utils.FreeTcpPortFinder.findFreeTcpPorts
import js7.data.Problems.ItemIsStillReferencedProblem
import js7.data.agent.AgentRefStateEvent.{AgentCouplingFailed, AgentDedicated, AgentReady}
import js7.data.agent.Problems.{AgentNotDedicatedProblem, AgentRunIdMismatchProblem}
import js7.data.agent.{AgentPath, AgentRef}
import js7.data.item.BasicItemEvent.ItemDeleted
import js7.data.item.ItemOperation.{AddOrChangeSigned, AddOrChangeSimple, AddVersion, DeleteSimple, RemoveVersioned}
import js7.data.item.VersionId
import js7.data.order.{FreshOrder, OrderId}
import js7.data.subagent.{SubagentId, SubagentItem}
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.UpdateAgentRefsTest.*
import js7.tests.jobs.EmptyJob
import js7.tests.testenv.ControllerTestUtils.newControllerApi
import js7.tests.testenv.{DirectorEnv, DirectoryProviderForScalaTest}
import scala.annotation.tailrec
import scala.language.unsafeNulls

final class UpdateAgentRefsTest extends OurTestSuite, DirectoryProviderForScalaTest:

  private given IORuntime = ioRuntime

  override protected def controllerConfig = config"""
    js7.auth.users.TEST-USER.permissions = [ UpdateItem ]
    js7.controller.agent-driver.command-batch-delay = 0ms
    js7.controller.agent-driver.event-buffer-delay = 5ms
    """

  override protected def agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = on"""

  protected val agentPaths = Nil
  protected val items = Nil

  private lazy val agentPort1 :: agentPort2 :: agentPort3 :: Nil = findFreeTcpPorts(3): @unchecked
  private lazy val agentEnv = new DirectorEnv(
    SubagentItem(
      SubagentId(agentPath.string + "-0"), agentPath, disabled = primarySubagentsDisabled,
      uri = Uri(s"http://127.0.0.1:$agentPort1")),
    directoryProvider.directory,
    name = "AGENT",
    extraConfig = agentConfig)

  private lazy val controller = directoryProvider.newController()

  private lazy val controllerApi = newControllerApi(controller, Some(directoryProvider.controllerEnv.userAndPassword))
  private lazy val eventWatch = controller.eventWatch
  private lazy val outdatedState = agentEnv.stateDir.resolveSibling(Paths.get("state~"))

  override def afterAll() =
    try
      controllerApi.stop.await(99.s)
      controller.terminate().await(99.s)
    finally
      super.afterAll()

  private val agentRef = AgentRef(agentPath, directors = Seq(subagentId))

  "Add and delete an Agent" - {
    var agent: TestAgent = null

    "Add AgentRef and run an order" in:
      directoryProvider.prepareAgentFiles(agentEnv)

      val subagentItem = SubagentItem(subagentId, agentPath, Uri(s"http://127.0.0.1:$agentPort1"))
      agent = TestAgent.start(agentEnv.agentConf).await(99.s)

      controllerApi
        .updateItems(
          Stream(
            AddOrChangeSimple(agentRef),
            AddOrChangeSimple(subagentItem),
            AddVersion(v1),
            AddOrChangeSigned(toSignedString(workflow.withVersion(v1)))))
        .await(99.s).orThrow
      controller.runOrder(FreshOrder(OrderId("🔷"), workflow.path, deleteWhenTerminated = true))

    "Delete AgentRef" in:
      copyDirectoryContent(agentEnv.stateDir, outdatedState)

      assert(controllerApi.updateItems(Stream(DeleteSimple(agentPath))).await(99.s) ==
        Left(Problem.combine(
          ItemIsStillReferencedProblem(agentPath, subagentId),
          ItemIsStillReferencedProblem(agentPath, workflow.path ~ v1))))

      val eventId = eventWatch.lastAddedEventId

      controllerApi.updateItems(Stream(
        DeleteSimple(agentPath),
        DeleteSimple(subagentId),
        AddVersion(VersionId("DELETE")),
        RemoveVersioned(workflow.path))
      ).await(99.s).orThrow

      eventWatch.await[ItemDeleted](_.event.key == agentPath, after = eventId)
      agent.untilTerminated.await(99.s)
  }

  "Add AgentRef again: Agent's journal should be new due to implicit Reset" in:
    agentEnv.testAgentResource.useSync(99.s): agent =>
      val eventId = eventWatch.lastAddedEventId
      val versionId = VersionId("AGAIN")
      val subagentItem = SubagentItem(subagentId, agentPath, Uri(s"http://127.0.0.1:$agentPort1"))

      @tailrec def loop(n: Int): Unit =
        val checked = controllerApi
          .updateItems(
            Stream(
              AddOrChangeSimple(agentRef),
              AddOrChangeSimple(subagentItem),
              AddVersion(versionId),
              AddOrChangeSigned(toSignedString(workflow.withVersion(versionId)))))
          .await(99.s)
          if n > 0 && checked.left.exists(_.toString.contains:
            "AgentDrivers for the following Agents are still running — please retry after some seconds:")
          then
            sleep(1.s)
            loop(n - 1)
          else
            checked.orThrow
      loop(2)

      eventWatch.await[AgentDedicated](after = eventId)
      eventWatch.await[AgentReady](after = eventId)
      controller.runOrder(FreshOrder(OrderId("AGAIN"), workflow.path))

  "Change Directors's URI to an unreachable address" in:
    val eventId = eventWatch.lastAddedEventId
    val subagentItem = SubagentItem(subagentId, agentPath, Uri("http://127.0.0.0:0"))
    controllerApi.updateUnsignedSimpleItems(Seq(subagentItem)).await(99.s).orThrow
    controller.addOrderBlocking(FreshOrder(OrderId("🔺"), workflow.path))
    eventWatch.await[AgentCouplingFailed](after = eventId)

  "Change Directors's URI and keep Agent's state (move the Agent)" in:
    val subagentItem = SubagentItem(subagentId, agentPath, Uri(s"http://127.0.0.1:$agentPort2"))
    TestAgent.resource(
      agentEnv.agentConf.copy(
        subagentConf = agentEnv.agentConf.subagentConf.copy(
          webServerPorts = List(WebServerPort.localhost(agentPort2))))
    ).useSync(99.s): agent =>
      controllerApi.updateUnsignedSimpleItems(Seq(subagentItem)).await(99.s).orThrow
      controller.runOrder(FreshOrder(OrderId("🔶"), workflow.path))

  "Coupling fails with outdated Director" in:
    deleteDirectoryRecursively(agentEnv.stateDir)
    move(outdatedState, agentEnv.stateDir)
    val eventId = eventWatch.lastAddedEventId

    TestAgent.resource(
      agentEnv.agentConf.copy(
        subagentConf = agentEnv.agentConf.subagentConf.copy(
          webServerPorts = List(WebServerPort.localhost(agentPort2))))
    ).useSync(99.s): agent =>
      // TODO May timeout due to repeated
      //  "Coupling failed: UnknownEventId: An unknown EventId has been requested"
      eventWatch.await[AgentCouplingFailed](
        _.event.problem == AgentRunIdMismatchProblem(agentPath),
        after = eventId)

  "Change Directors's URI and start Agent with clean state: fails" in:
    val subagentItem = SubagentItem(subagentId, agentPath, Uri(s"http://127.0.0.1:$agentPort3"))
    // DELETE AGENT'S STATE DIRECTORY
    deleteDirectoryContentRecursively(agentEnv.stateDir)
    TestAgent.resource(
      agentEnv.agentConf.copy(
        subagentConf = agentEnv.agentConf.subagentConf.copy(
          webServerPorts = List(WebServerPort.localhost(agentPort3))))
    ).useSync(99.s): agent =>
      val eventId = eventWatch.lastAddedEventId
      controllerApi.updateUnsignedSimpleItems(Seq(subagentItem)).await(99.s).orThrow
      eventWatch.await[AgentCouplingFailed](
        _.event.problem == AgentNotDedicatedProblem,
        after = eventId)


object UpdateAgentRefsTest:
  private val agentPath = AgentPath("AGENT")
  private val subagentId = SubagentId("SUBAGENT")
  private val v1 = VersionId("1")
  private val workflow = Workflow(WorkflowPath("WORKFLOW"), Vector(
    EmptyJob.execute(agentPath)))
