package js7.tests

import js7.agent.RunningAgent
import js7.agent.data.Problems.UnknownController
import js7.base.problem.Checked._
import js7.base.time.ScalaTime._
import js7.base.web.Uri
import js7.common.akkahttp.web.data.WebServerPort
import js7.common.process.Processes.{ShellFileExtension => sh}
import js7.common.scalautil.FileUtils.deleteDirectoryContentRecursively
import js7.common.scalautil.Futures.implicits._
import js7.common.scalautil.MonixUtils.syntax._
import js7.common.utils.FreeTcpPortFinder.findFreeTcpPorts
import js7.controller.data.events.AgentRefStateEvent.AgentCouplingFailed
import js7.data.agent.{AgentId, AgentRef}
import js7.data.controller.ControllerId
import js7.data.job.{PathExecutable, RelativePathExecutable}
import js7.data.order.{FreshOrder, OrderId}
import js7.data.workflow.instructions.Execute
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.UpdateAgentRefsTest._
import js7.tests.testenv.DirectoryProvider.script
import js7.tests.testenv.{DirectoryProvider, DirectoryProviderForScalaTest}
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AnyFreeSpec

final class UpdateAgentRefsTest extends AnyFreeSpec with DirectoryProviderForScalaTest
{
  protected val agentIds = Nil
  protected val versionedItems = workflow :: Nil
  private lazy val agentPort1 :: agentPort2 :: agentPort3 :: Nil = findFreeTcpPorts(3)
  private lazy val agentFileTree = new DirectoryProvider.AgentTree(directoryProvider.directory, agentId, "AGENT", agentPort1)
  private lazy val controller = directoryProvider.startController() await 99.s
  private var agent: RunningAgent = null

  override def afterAll() = {
    controller.terminate() await 99.s
    super.afterAll()
  }

  "Standard operation" in {
    directoryProvider.prepareAgentFiles(agentFileTree)
    agentFileTree.writeExecutable(RelativePathExecutable(s"EXECUTABLE$sh"), script(0.s))

    val agentRef = AgentRef(agentId, Uri(s"http://127.0.0.1:$agentPort1"))
    agent = RunningAgent.startForTest(agentFileTree.agentConfiguration) await 99.s

    controller.updateSimpleItemsAsSystemUser(Seq(agentRef)).await(99.s).orThrow
    controller.runOrder(FreshOrder(OrderId("🔵"), workflow.path))
  }

  "Change Agent's URI and keep Agent's state" in {
    agent.terminate() await 99.s
    val agentRef = AgentRef(agentId, Uri(s"http://127.0.0.1:$agentPort2"))
    agent = RunningAgent.startForTest(
      agentFileTree.agentConfiguration.copy(
        webServerPorts = List(WebServerPort.localhost(agentPort2)))
    ) await 99.s
    controller.updateSimpleItemsAsSystemUser(Seq(agentRef)).await(99.s).orThrow
    controller.runOrder(FreshOrder(OrderId("🔶"), workflow.path))
  }

  "Change Agent's URI and start Agent with clean state: should fail" in {
    agent.terminate() await 99.s
    val agentRef = AgentRef(agentId, Uri(s"http://127.0.0.1:$agentPort3"))
    // DELETE AGENT'S STATE DIRECTORY
    deleteDirectoryContentRecursively(agentFileTree.stateDir)
    agent = RunningAgent.startForTest(
      agentFileTree.agentConfiguration.copy(
        webServerPorts = List(WebServerPort.localhost(agentPort3)))
    ) await 99.s
    val beforeUpdate = controller.eventWatch.lastFileTornEventId
    controller.updateSimpleItemsAsSystemUser(Seq(agentRef)).await(99.s).orThrow
    controller.addOrderBlocking(FreshOrder(OrderId("❌"), workflow.path))
    controller.eventWatch.await[AgentCouplingFailed](
      _.event.problem == UnknownController(ControllerId("Controller")),
      after = beforeUpdate)
    agent.terminate() await 99.s
  }
}

object UpdateAgentRefsTest
{
  private val agentId = AgentId("AGENT")
  private val workflow = Workflow(WorkflowPath("WORKFLOW"), Vector(
    Execute(WorkflowJob(agentId, PathExecutable(s"EXECUTABLE$sh")))))
}
