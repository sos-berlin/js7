package js7.tests

import js7.agent.RunningAgent
import js7.agent.data.Problems.AgentRunIdMismatchProblem
import js7.base.io.file.FileUtils.deleteDirectoryContentRecursively
import js7.base.io.process.Processes.{ShellFileExtension => sh}
import js7.base.problem.Checked._
import js7.base.thread.Futures.implicits._
import js7.base.thread.MonixBlocking.syntax._
import js7.base.time.ScalaTime._
import js7.base.web.Uri
import js7.common.akkahttp.web.data.WebServerPort
import js7.common.utils.FreeTcpPortFinder.findFreeTcpPorts
import js7.data.agent.AgentRefStateEvent.AgentCouplingFailed
import js7.data.agent.{AgentPath, AgentRef}
import js7.data.item.ItemOperation.{AddOrChangeSigned, AddOrChangeSimple, AddVersion}
import js7.data.item.VersionId
import js7.data.job.{PathExecutable, RelativePathExecutable}
import js7.data.order.{FreshOrder, OrderId}
import js7.data.workflow.instructions.Execute
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.UpdateAgentRefsTest._
import js7.tests.testenv.DirectoryProvider.script
import js7.tests.testenv.{DirectoryProvider, DirectoryProviderForScalaTest}
import monix.execution.Scheduler.Implicits.global
import monix.reactive.Observable
import org.scalatest.freespec.AnyFreeSpec

final class UpdateAgentRefsTest extends AnyFreeSpec with DirectoryProviderForScalaTest
{
  protected val agentPaths = Nil
  protected val items = Nil
  private lazy val agentPort1 :: agentPort2 :: agentPort3 :: Nil = findFreeTcpPorts(3)
  private lazy val agentFileTree = new DirectoryProvider.AgentTree(directoryProvider.directory, agentPath, "AGENT", agentPort1)
  private lazy val controller = directoryProvider.startController() await 99.s
  private var agent: RunningAgent = null

  override def afterAll() = {
    controller.terminate() await 99.s
    super.afterAll()
  }

  "Standard operation" in {
    directoryProvider.prepareAgentFiles(agentFileTree)
    agentFileTree.writeExecutable(RelativePathExecutable(s"EXECUTABLE$sh"), script(0.s))

    val agentRef = AgentRef(agentPath, Uri(s"http://127.0.0.1:$agentPort1"))
    agent = RunningAgent.startForTest(agentFileTree.agentConfiguration) await 99.s

    val versionId = VersionId("1")
    controller
      .updateItemsAsSystemUser(
        Observable(
          AddOrChangeSimple(agentRef),
          AddVersion(versionId),
          AddOrChangeSigned(sign(workflow withVersion versionId).signedString)))
      .await(99.s).orThrow
    controller.runOrder(FreshOrder(OrderId("🔵"), workflow.path))
  }

  "Change Agent's URI and keep Agent's state" in {
    agent.terminate() await 99.s
    val agentRef = AgentRef(agentPath, Uri(s"http://127.0.0.1:$agentPort2"))
    agent = RunningAgent.startForTest(
      agentFileTree.agentConfiguration.copy(
        webServerPorts = List(WebServerPort.localhost(agentPort2)))
    ) await 99.s
    controller.updateUnsignedSimpleItemsAsSystemUser(Seq(agentRef)).await(99.s).orThrow
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
    controller.updateUnsignedSimpleItemsAsSystemUser(Seq(agentRef)).await(99.s).orThrow
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
  private val workflow = Workflow(WorkflowPath("WORKFLOW"), Vector(
    Execute(WorkflowJob(agentPath, PathExecutable(s"EXECUTABLE$sh")))))
}
