package js7.agent.tests

import java.nio.file.Files.createDirectory
import java.nio.file.Path
import js7.agent.TestAgent
import js7.agent.configuration.AgentConfiguration
import js7.agent.data.commands.AgentCommand
import js7.agent.data.commands.AgentCommand.{AttachItem, AttachOrder, AttachSignedItem, DedicateAgentDirector}
import js7.agent.tests.AgentTest.*
import js7.agent.tests.TestAgentDirectoryProvider.provideAgentDirectory
import js7.base.auth.SimpleUser
import js7.base.io.file.FileUtils.WorkingDirectory
import js7.base.io.file.FileUtils.syntax.*
import js7.base.io.process.Processes.ShellFileExtension as sh
import js7.base.system.OperatingSystem.isWindows
import js7.base.test.OurTestSuite
import js7.base.thread.MonixBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.base.web.Uri
import js7.core.command.CommandMeta
import js7.data.agent.AgentPath
import js7.data.controller.{ControllerId, ControllerRunId}
import js7.data.event.JournalId
import js7.data.job.RelativePathExecutable
import js7.data.order.OrderEvent.OrderProcessed
import js7.data.order.{Order, OrderId, Outcome}
import js7.data.subagent.{SubagentId, SubagentItem}
import js7.data.value.{NumberValue, StringValue}
import js7.data.workflow.instructions.Execute
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.position.Position
import js7.data.workflow.test.TestSetting.TestAgentPath
import js7.data.workflow.{Workflow, WorkflowPath}
import monix.execution.Scheduler.Implicits.traced

/**
  * @author Joacim Zschimmer
  */
final class AgentTest extends OurTestSuite with AgentTester:
  "work/http-uri" in:
    assert((agentConfiguration.workDirectory / "http-uri").contentString ==
      s"${agent.localUri}/subagent")
    agent.terminate() await 99.s

  "Job working directory" - {
    for ((testName, toWorkingDirectory) <-
           Array[(String, Path => Path)](
             ("default", _ => WorkingDirectory),
             ("not default", _ / "working")))
      testName in :
        provideAgentDirectory { directory =>
          createDirectory(directory / "working")
          val workingDirectory = toWorkingDirectory(directory).toRealPath()
          TestPathExecutable.toFile(directory / "config" / "executables").writeUtf8Executable(TestScript)
          var agentConf = AgentConfiguration.forTest(directory, name = "AgentTest")
          if directory != WorkingDirectory then
            agentConf = agentConf.copy(
              subagentConf = agentConf.subagentConf.copy(
                jobWorkingDirectory = workingDirectory))
          TestAgent.blockingRun(agentConf, 99.s) { agent =>
            val agentApi = agent.untilReady.await(99.s).api.apply(CommandMeta(TestUser))
            val controllerRunId = ControllerRunId(JournalId.random())
            agentApi
              .commandExecute(
                DedicateAgentDirector(Seq(subagentId), controllerId, controllerRunId, agentPath))
              .await(99.s).orThrow
            agentApi
              .commandExecute(
                AttachItem(SubagentItem(subagentId, agentPath, Uri("https://127.0.0.1:0"))))
              .await(99.s).orThrow

            assert(agentApi.commandExecute(AttachSignedItem(itemSigner.sign(TestWorkflow))).await(99.s)
              == Right(AgentCommand.Response.Accepted))
            val order = Order(OrderId("TEST"), TestWorkflow.id /: Position(0), Order.Ready)
            assert(agentApi.commandExecute(AttachOrder(order, TestAgentPath)).await(99.s)
              == Right(AgentCommand.Response.Accepted))
            val orderProcessed = agent.eventWatch.await[OrderProcessed]().head.value.event
            assert(orderProcessed.outcome == Outcome.Succeeded(Map("returnCode" -> NumberValue(0), "WORKDIR" -> StringValue(workingDirectory.toString))))
            agent.terminate() await 99.s
          }
        }
  }

object AgentTest:
  private val controllerId = ControllerId("CONTROLLER")
  private val TestUser = SimpleUser(controllerId.toUserId)
  private val agentPath = AgentPath("AGENT")
  private val subagentId = SubagentId("SUBAGENT")

  private val TestScript =
    if isWindows then """
      |@echo off
      |set /p dummy=WORKDIR= <nul >%JS7_RETURN_VALUES%
      |cd >>%JS7_RETURN_VALUES%
      |""".stripMargin
    else """
      |echo TEST TEST
      |echo "WORKDIR=$(pwd)" >$JS7_RETURN_VALUES
      |""".stripMargin

  private val TestPathExecutable = RelativePathExecutable(s"TEST$sh")

  private val TestWorkflow = Workflow.of(
    WorkflowPath("WORKFLOW") ~ "VERSION",
    Execute(WorkflowJob(TestAgentPath, TestPathExecutable)))
