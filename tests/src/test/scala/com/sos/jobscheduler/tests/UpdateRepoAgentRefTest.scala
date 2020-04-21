package com.sos.jobscheduler.tests

import com.sos.jobscheduler.agent.RunningAgent
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.base.web.Uri
import com.sos.jobscheduler.common.akkahttp.web.data.WebServerPort
import com.sos.jobscheduler.common.process.Processes.{ShellFileExtension => sh}
import com.sos.jobscheduler.common.scalautil.FileUtils.deleteDirectoryContentRecursively
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.scalautil.MonixUtils.syntax._
import com.sos.jobscheduler.common.utils.FreeTcpPortFinder.findFreeTcpPorts
import com.sos.jobscheduler.data.agent.{AgentRef, AgentRefPath}
import com.sos.jobscheduler.data.filebased.VersionId
import com.sos.jobscheduler.data.job.ExecutablePath
import com.sos.jobscheduler.data.order.{FreshOrder, OrderId}
import com.sos.jobscheduler.data.workflow.instructions.Execute
import com.sos.jobscheduler.data.workflow.instructions.executable.WorkflowJob
import com.sos.jobscheduler.data.workflow.{Workflow, WorkflowPath}
import com.sos.jobscheduler.master.data.events.MasterAgentEvent.AgentCouplingFailed
import com.sos.jobscheduler.tests.UpdateRepoAgentRefTest._
import com.sos.jobscheduler.tests.testenv.DirectoryProvider.script
import com.sos.jobscheduler.tests.testenv.{DirectoryProvider, DirectoryProviderForScalaTest}
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AnyFreeSpec

final class UpdateRepoAgentRefTest extends AnyFreeSpec with DirectoryProviderForScalaTest
{
  protected val agentRefPaths = Nil
  protected val fileBased = workflow :: Nil
  private lazy val agentPort1 :: agentPort2 :: agentPort3 :: Nil = findFreeTcpPorts(3)
  private lazy val agentFileTree = new DirectoryProvider.AgentTree(directoryProvider.directory, agentRefPath, "AGENT", agentPort1)
  private lazy val master = directoryProvider.startMaster() await 99.s
  private var agentRef: AgentRef = null
  private var agent: RunningAgent = null

  override def afterAll() = {
    master.terminate() await 99.s
    super.afterAll()
  }

  "Standard operation" in {
    val v1 = VersionId("1")
    directoryProvider.prepareAgentFiles(agentFileTree)
    agentFileTree.writeExecutable(ExecutablePath(s"/EXECUTABLE$sh"), script(0.s))

    agentRef = AgentRef(agentRefPath, Uri(s"http://127.0.0.1:$agentPort1"))
    agent = RunningAgent.startForTest(agentFileTree.agentConfiguration) await 99.s

    directoryProvider.updateRepo(master, v1, List(agentRef))
    master.runOrder(FreshOrder(OrderId("🔵"), workflow.path))
  }

  "Change Agent's URI and keep Agent's state" in {
    val v2 = VersionId("2")
    agent.terminate() await 99.s
    agentRef = AgentRef(agentRefPath, Uri(s"http://127.0.0.1:$agentPort2"))
    agent = RunningAgent.startForTest(
      agentFileTree.agentConfiguration.copy(
        webServerPorts = List(WebServerPort.localhost(agentPort2)))
    ) await 99.s
    directoryProvider.updateRepo(master, v2, List(agentRef))
    master.runOrder(FreshOrder(OrderId("🔶"), workflow.path))
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
    val beforeUpdate = master.eventWatch.lastFileTornEventId
    directoryProvider.updateRepo(master, v3, List(agentRef))
    master.addOrder(FreshOrder(OrderId("❌"), workflow.path)) await 99.s
    master.eventWatch.await[AgentCouplingFailed](
      _.event.problem == Problem("Agent does not know MasterId: Master"),
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
