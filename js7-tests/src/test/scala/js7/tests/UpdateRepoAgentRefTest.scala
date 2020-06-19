package js7.tests

import js7.agent.RunningAgent
import js7.agent.data.Problems.UnknownController
import js7.base.time.ScalaTime._
import js7.base.web.Uri
import js7.common.akkahttp.web.data.WebServerPort
import js7.common.process.Processes.{ShellFileExtension => sh}
import js7.common.scalautil.FileUtils.deleteDirectoryContentRecursively
import js7.common.scalautil.Futures.implicits._
import js7.common.scalautil.MonixUtils.syntax._
import js7.common.utils.FreeTcpPortFinder.findFreeTcpPorts
import js7.controller.data.events.ControllerAgentEvent.AgentCouplingFailed
import js7.data.agent.{AgentRef, AgentRefPath}
import js7.data.controller.ControllerId
import js7.data.filebased.VersionId
import js7.data.job.ExecutablePath
import js7.data.order.{FreshOrder, OrderId}
import js7.data.workflow.instructions.Execute
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.UpdateRepoAgentRefTest._
import js7.tests.testenv.DirectoryProvider.script
import js7.tests.testenv.{DirectoryProvider, DirectoryProviderForScalaTest}
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AnyFreeSpec

final class UpdateRepoAgentRefTest extends AnyFreeSpec with DirectoryProviderForScalaTest
{
  protected val agentRefPaths = Nil
  protected val fileBased = workflow :: Nil
  private lazy val agentPort1 :: agentPort2 :: agentPort3 :: Nil = findFreeTcpPorts(3)
  private lazy val agentFileTree = new DirectoryProvider.AgentTree(directoryProvider.directory, agentRefPath, "AGENT", agentPort1)
  private lazy val controller = directoryProvider.startController() await 99.s
  private var agentRef: AgentRef = null
  private var agent: RunningAgent = null

  override def afterAll() = {
    controller.terminate() await 99.s
    super.afterAll()
  }

  "Standard operation" in {
    val v1 = VersionId("1")
    directoryProvider.prepareAgentFiles(agentFileTree)
    agentFileTree.writeExecutable(ExecutablePath(s"/EXECUTABLE$sh"), script(0.s))

    agentRef = AgentRef(agentRefPath, Uri(s"http://127.0.0.1:$agentPort1"))
    agent = RunningAgent.startForTest(agentFileTree.agentConfiguration) await 99.s

    directoryProvider.updateRepo(controller, v1, List(agentRef))
    controller.runOrder(FreshOrder(OrderId("🔵"), workflow.path))
  }

  "Change Agent's URI and keep Agent's state" in {
    val v2 = VersionId("2")
    agent.terminate() await 99.s
    agentRef = AgentRef(agentRefPath, Uri(s"http://127.0.0.1:$agentPort2"))
    agent = RunningAgent.startForTest(
      agentFileTree.agentConfiguration.copy(
        webServerPorts = List(WebServerPort.localhost(agentPort2)))
    ) await 99.s
    directoryProvider.updateRepo(controller, v2, List(agentRef))
    controller.runOrder(FreshOrder(OrderId("🔶"), workflow.path))
  }

  "Change Agent's URI and start Agent with clean state: should fail" in {
    val v3 = VersionId("3")
    agent.terminate() await 99.s
    agentRef = AgentRef(agentRefPath, Uri(s"http://127.0.0.1:$agentPort3"))
    // DELETE AGENT'S STATE DIRECTORY
    deleteDirectoryContentRecursively(agentFileTree.stateDir)
    agent = RunningAgent.startForTest(
      agentFileTree.agentConfiguration.copy(
        webServerPorts = List(WebServerPort.localhost(agentPort3)))
    ) await 99.s
    val beforeUpdate = controller.eventWatch.lastFileTornEventId
    directoryProvider.updateRepo(controller, v3, List(agentRef))
    controller.addOrder(FreshOrder(OrderId("❌"), workflow.path)) await 99.s
    controller.eventWatch.await[AgentCouplingFailed](
      _.event.problem == UnknownController(ControllerId("Controller")),
      after = beforeUpdate)
    agent.terminate() await 99.s
  }
}

object UpdateRepoAgentRefTest
{
  private val agentRefPath = AgentRefPath("/AGENT")
  private val workflow = Workflow(WorkflowPath("/WORKFLOW"), Vector(
    Execute(WorkflowJob(agentRefPath, ExecutablePath(s"/EXECUTABLE$sh")))))
}
