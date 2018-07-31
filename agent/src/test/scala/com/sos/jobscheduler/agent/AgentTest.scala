package com.sos.jobscheduler.agent

import com.sos.jobscheduler.agent.AgentTest._
import com.sos.jobscheduler.agent.command.CommandMeta
import com.sos.jobscheduler.agent.configuration.AgentConfiguration
import com.sos.jobscheduler.agent.configuration.Akkas.newActorSystem
import com.sos.jobscheduler.agent.data.commands.AgentCommand
import com.sos.jobscheduler.agent.data.commands.AgentCommand.{AttachOrder, RegisterAsMaster}
import com.sos.jobscheduler.agent.scheduler.job.{JobConfiguration, JobScript}
import com.sos.jobscheduler.agent.test.AgentTester
import com.sos.jobscheduler.agent.test.TestAgentDirectoryProvider.provideAgentDirectory
import com.sos.jobscheduler.base.auth.SimpleUser
import com.sos.jobscheduler.base.circeutils.CirceUtils.RichJson
import com.sos.jobscheduler.base.utils.MapDiff
import com.sos.jobscheduler.common.scalautil.Closers.withCloser
import com.sos.jobscheduler.common.scalautil.FileUtils.WorkingDirectory
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.MonixUtils.ops._
import com.sos.jobscheduler.common.system.OperatingSystem.isWindows
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.data.filebased.SourceType
import com.sos.jobscheduler.data.job.JobPath
import com.sos.jobscheduler.data.master.MasterId
import com.sos.jobscheduler.data.order.OrderEvent.OrderProcessed
import com.sos.jobscheduler.data.order.{Order, OrderId}
import com.sos.jobscheduler.data.workflow.instructions.Job
import com.sos.jobscheduler.data.workflow.test.TestSetting.TestAgentPath
import com.sos.jobscheduler.data.workflow.{Workflow, WorkflowPath}
import io.circe.syntax.EncoderOps
import java.nio.file.{Files, Path}
import monix.execution.Scheduler.Implicits.global
import org.scalatest.FreeSpec
import org.scalatest.Matchers._
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
final class AgentTest extends FreeSpec with AgentTester
{
  "Job working directory" - {
    for ((testName, toWorkingDirectory) ←
           Array[(String, Path ⇒ Path)](
             ("default", _ ⇒ WorkingDirectory),
             ("not default", _ / "working")))
      testName in {
        provideAgentDirectory { directory ⇒
          Files.createDirectory(directory / "working")
          val workingDirectory = toWorkingDirectory(directory).toRealPath()
          val jobDir = directory / "config" / "live"
          (jobDir resolve TestJob.path.toFile(SourceType.Json)).contentString = TestJob.asJson.toPrettyString
          var agentConf = AgentConfiguration.forTest(directory)
          if (directory != WorkingDirectory) {
            agentConf = agentConf.copy(jobWorkingDirectory = workingDirectory)
          }
          RunningAgent.run(agentConf, timeout = Some(99.s)) { agent ⇒
            withCloser { implicit closer ⇒
              implicit val actorSystem = newActorSystem(getClass.getSimpleName)
              val agentApi = agent.api(CommandMeta(TestUser))
              agentApi.commandExecute(RegisterAsMaster) await 99.s shouldEqual AgentCommand.Accepted

              val order = Order(OrderId("TEST"), TestWorkflow.id, Order.Ready)
              agentApi.commandExecute(AttachOrder(order, TestAgentPath % "(initial)", TestWorkflow)) await 99.s shouldEqual AgentCommand.Accepted
              val eventWatch = agentApi.eventWatchForMaster(TestMasterId).await(99.seconds)
              val orderProcessed = eventWatch.await[OrderProcessed]().head.value.event
              assert(orderProcessed.variablesDiff == MapDiff(Map("WORKDIR" → workingDirectory.toString)))
              agent.terminate() await 99.s
            }
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

  private val TestJob =
    JobConfiguration(JobPath("/JOB") % "(initial)", JobScript(TestScript))

  private val TestWorkflow = Workflow.of(
    WorkflowPath("/WORKFLOW") % "VERSION",
    Job(TestJob.path, TestAgentPath))
}
