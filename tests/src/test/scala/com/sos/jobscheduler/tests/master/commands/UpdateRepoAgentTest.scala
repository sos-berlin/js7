package com.sos.jobscheduler.tests.master.commands

import com.sos.jobscheduler.agent.RunningAgent
import com.sos.jobscheduler.agent.configuration.AgentConfiguration
import com.sos.jobscheduler.base.auth.{UserAndPassword, UserId}
import com.sos.jobscheduler.base.generic.SecretString
import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.base.utils.AutoClosing.autoClosing
import com.sos.jobscheduler.common.http.AkkaHttpClient.HttpException
import com.sos.jobscheduler.common.log.ScribeUtils
import com.sos.jobscheduler.common.scalautil.FileUtils.syntax._
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.scalautil.MonixUtils.syntax.RichTask
import com.sos.jobscheduler.common.utils.FreeTcpPortFinder.findFreeTcpPort
import com.sos.jobscheduler.data.agent.{AgentRef, AgentRefPath}
import com.sos.jobscheduler.data.filebased.VersionId
import com.sos.jobscheduler.data.job.ExecutablePath
import com.sos.jobscheduler.data.order.OrderEvent.OrderFinished
import com.sos.jobscheduler.data.order.{FreshOrder, OrderId}
import com.sos.jobscheduler.data.workflow.WorkflowPath
import com.sos.jobscheduler.data.workflow.parser.WorkflowParser
import com.sos.jobscheduler.master.RunningMaster
import com.sos.jobscheduler.master.data.MasterCommand
import com.sos.jobscheduler.master.data.MasterCommand.UpdateRepo
import com.sos.jobscheduler.tests.master.commands.UpdateRepoAgentTest._
import com.sos.jobscheduler.tests.testenv.DirectoryProvider
import monix.execution.Scheduler.Implicits.global
import scala.concurrent.duration._
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class UpdateRepoAgentTest extends AnyFreeSpec
{
  ScribeUtils.coupleScribeWithSlf4j()

  "MasterCommand.UpdateRepo" in {
    autoClosing(new DirectoryProvider(agentRefPath :: Nil, workflow :: Nil, testName = Some("UpdateRepoAgentTest"))) { provider =>
      (provider.master.configDir / "private" / "private.conf") ++=
        """jobscheduler.auth.users {
          |  UpdateRepoAgentTest {
          |    password = "plain:TEST-PASSWORD"
          |    permissions = [ UpdateRepo ]
          |  }
          |}
          |""".stripMargin
      provider.agentToTree(agentRefPath).writeExecutable(ExecutablePath("/SCRIPT.cmd"), ":")

      // Start Agent before Master to bind the reserved TCP port early, and the Master needs not to wait
      val agent1 = provider.startAgents().await(99.seconds).head
      var agent2: RunningAgent = null
      provider.runMaster() { master =>
        master.httpApi.login(Some(UserAndPassword(UserId("UpdateRepoAgentTest"), SecretString("TEST-PASSWORD")))) await 99.seconds
        runOrder(master, OrderId("🔺"))
        agent1.terminate() await 99.seconds

        for (i <- 1 to 3) {
          if (agent2 != null) agent2.terminate() await 99.seconds
          // Start a new Agent with same state but a (hopefully) different HTTP port
          val port = findFreeTcpPort()
          agent2 = RunningAgent.startForTest(AgentConfiguration.forTest(
            provider.agents.head.directory,
            httpPort = Some(port))
          ).await(99.seconds)

          val versionId = VersionId(s"$i")
          val agentRef = AgentRef(agentRefPath ~ versionId, uri = agent2.localUri)
          executeCommand(master, UpdateRepo(versionId, provider.fileBasedSigner.sign(agentRef) :: Nil))

          runOrder(master, OrderId(s"🔵-$i"))
        }
      }

      // Master recovery
      provider.runMaster() { master =>
        runOrder(master, OrderId("⭕"))
      }

      agent2.terminate() await 99.seconds
    }
  }

  private def executeCommand(master: RunningMaster, cmd: MasterCommand): Checked[cmd.Response] =
    master.httpApi.executeCommand(cmd).map(Right.apply)
      .onErrorRecover { case e: HttpException if e.problem.isDefined => Left(e.problem.get) }
      .await(99.seconds)
}

object UpdateRepoAgentTest
{
  private val agentRefPath = AgentRefPath("/AGENT")
  private val workflow = WorkflowParser.parse(WorkflowPath("/WORKFLOW"),
     """define workflow {
          execute executable="/SCRIPT.cmd", agent="/AGENT";
        }"""
  ).orThrow

  private def runOrder(master: RunningMaster, orderId: OrderId): Unit = {
    master.addOrderBlocking(FreshOrder(orderId, workflow.path))
    master.eventWatch.await[OrderFinished](predicate = _.key == orderId)
  }
}
