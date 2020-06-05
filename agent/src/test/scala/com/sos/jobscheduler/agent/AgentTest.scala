package js7.agent

import js7.agent.AgentTest._
import js7.agent.configuration.AgentConfiguration
import js7.agent.data.commands.AgentCommand
import js7.agent.data.commands.AgentCommand.{AttachOrder, RegisterAsMaster}
import js7.agent.tests.AgentTester
import js7.agent.tests.TestAgentDirectoryProvider.provideAgentDirectory
import js7.base.auth.SimpleUser
import js7.base.time.ScalaTime._
import js7.common.process.Processes.{ShellFileExtension => sh}
import js7.common.scalautil.FileUtils.WorkingDirectory
import js7.common.scalautil.FileUtils.syntax._
import js7.common.scalautil.MonixUtils.syntax._
import js7.common.system.OperatingSystem.isWindows
import js7.core.command.CommandMeta
import js7.data.agent.AgentRefPath
import js7.data.job.ExecutablePath
import js7.data.master.MasterId
import js7.data.order.OrderEvent.OrderProcessed
import js7.data.order.{Order, OrderId, Outcome}
import js7.data.workflow.instructions.Execute
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.test.TestSetting.TestAgentRefPath
import js7.data.workflow.{Workflow, WorkflowPath}
import java.nio.file.Files.createDirectory
import java.nio.file.Path
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AnyFreeSpec
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
final class AgentTest extends AnyFreeSpec with AgentTester
{
  "state/http-uri" in {
    assert((agentConfiguration.stateDirectory / "http-uri").contentString == s"${agent.localUri}/agent")
    agent.terminate() await 99.seconds
  }

  "Job working directory" - {
    for ((testName, toWorkingDirectory) <-
           Array[(String, Path => Path)](
             ("default", _ => WorkingDirectory),
             ("not default", _ / "working")))
      testName in {
        provideAgentDirectory { directory =>
          createDirectory(directory / "working")
          val workingDirectory = toWorkingDirectory(directory).toRealPath()
          TestExecutablePath.toFile(directory / "config" / "executables").writeExecutable(TestScript)
          var agentConf = AgentConfiguration.forTest(directory)
          if (directory != WorkingDirectory) {
            agentConf = agentConf.copy(jobWorkingDirectory = workingDirectory)
          }
          RunningAgent.run(agentConf, timeout = Some(99.s)) { agent =>
            val agentApi = agent.api(CommandMeta(TestUser))
            assert(agentApi.commandExecute(RegisterAsMaster(agentRefPath)).await(99.s).toOption.get
              .isInstanceOf[RegisterAsMaster.Response])

            val order = Order(OrderId("TEST"), TestWorkflow.id, Order.Ready)
            assert(agentApi.commandExecute(AttachOrder(order, TestAgentRefPath, fileBasedSigner.sign(TestWorkflow))).await(99.s)
              == Right(AgentCommand.Response.Accepted))
            val Right(eventWatch) = agentApi.eventWatchForMaster(TestMasterId).await(99.seconds)
            val orderProcessed = eventWatch.await[OrderProcessed]().head.value.event
            assert(orderProcessed.outcome == Outcome.Succeeded(Map("WORKDIR" -> workingDirectory.toString)))
            agent.terminate() await 99.s
          }
        }
      }
  }
}

object AgentTest {
  private val TestMasterId = MasterId("MASTER")
  private val TestUser = SimpleUser(TestMasterId.toUserId)
  private val agentRefPath = AgentRefPath("/AGENT")

  private val TestScript =
    if (isWindows) """
      |@echo off
      |set /p dummy=WORKDIR= <nul >%SCHEDULER_RETURN_VALUES%
      |cd >>%SCHEDULER_RETURN_VALUES%
      |""".stripMargin
    else """
      |echo TEST TEST
      |echo "WORKDIR=$(pwd)" >$SCHEDULER_RETURN_VALUES
      |""".stripMargin

  private val TestExecutablePath = ExecutablePath(s"/TEST$sh")

  private val TestWorkflow = Workflow.of(
    WorkflowPath("/WORKFLOW") ~ "VERSION",
    Execute(WorkflowJob(TestAgentRefPath, TestExecutablePath)))
}
