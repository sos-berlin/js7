package com.sos.jobscheduler.agent

import cats.data.Validated.Valid
import com.sos.jobscheduler.agent.AgentTest._
import com.sos.jobscheduler.agent.configuration.AgentConfiguration
import com.sos.jobscheduler.agent.data.commands.AgentCommand
import com.sos.jobscheduler.agent.data.commands.AgentCommand.{AttachOrder, RegisterAsMaster}
import com.sos.jobscheduler.agent.tests.AgentTester
import com.sos.jobscheduler.agent.tests.TestAgentDirectoryProvider.provideAgentDirectory
import com.sos.jobscheduler.base.auth.SimpleUser
import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.common.process.Processes.{ShellFileExtension => sh}
import com.sos.jobscheduler.common.scalautil.FileUtils.WorkingDirectory
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.MonixUtils.ops._
import com.sos.jobscheduler.common.system.OperatingSystem.isWindows
import com.sos.jobscheduler.core.command.CommandMeta
import com.sos.jobscheduler.core.crypt.silly.{SillySignature, SillySigner}
import com.sos.jobscheduler.core.filebased.FileBasedSigner
import com.sos.jobscheduler.data.job.ExecutablePath
import com.sos.jobscheduler.data.master.MasterId
import com.sos.jobscheduler.data.order.OrderEvent.OrderProcessed
import com.sos.jobscheduler.data.order.{Order, OrderId, Outcome}
import com.sos.jobscheduler.data.workflow.instructions.Execute
import com.sos.jobscheduler.data.workflow.instructions.executable.WorkflowJob
import com.sos.jobscheduler.data.workflow.test.TestSetting.TestAgentRefPath
import com.sos.jobscheduler.data.workflow.{Workflow, WorkflowPath}
import java.nio.file.Files.createDirectory
import java.nio.file.Path
import monix.execution.Scheduler.Implicits.global
import org.scalatest.FreeSpec
import org.scalatest.Matchers._
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
final class AgentTest extends FreeSpec with AgentTester
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
            agentApi.commandExecute(RegisterAsMaster) await 99.s shouldEqual Valid(AgentCommand.Response.Accepted)

            val order = Order(OrderId("TEST"), TestWorkflow.id, Order.Ready)
            assert(agentApi.commandExecute(AttachOrder(order, TestAgentRefPath, fileBasedSigner.sign(TestWorkflow))).await(99.s)
              == Valid(AgentCommand.Response.Accepted))
            val Valid(eventWatch) = agentApi.eventWatchForMaster(TestMasterId).await(99.seconds)
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
