package com.sos.jobscheduler.tests.master.commands

import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.agent.RunningAgent
import com.sos.jobscheduler.agent.configuration.AgentConfiguration
import com.sos.jobscheduler.base.auth.{UserAndPassword, UserId}
import com.sos.jobscheduler.base.generic.SecretString
import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.common.http.AkkaHttpClient.HttpException
import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.scalautil.MonixUtils.ops.RichTask
import com.sos.jobscheduler.common.utils.FreeTcpPortFinder.findRandomFreeTcpPort
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
import org.scalatest.FreeSpec
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
final class UpdateRepoAgentTest extends FreeSpec
{
  "MasterCommand.UpdateRepo" in {
    autoClosing(new DirectoryProvider(agentRefPath :: Nil, workflow :: Nil)) { provider =>
      (provider.master.config / "private" / "private.conf") ++=
        """jobscheduler.auth.users {
          |  UpdateRepoAgentTest {
          |    password = "plain:TEST-PASSWORD"
          |    permissions = [ UpdateRepo ]
          |  }
          |}
          |""".stripMargin
      provider.agentToTree(agentRefPath).writeExecutable(ExecutablePath("/SCRIPT.cmd"), ":")

      var agent2: RunningAgent = null
      provider.runMaster() { master =>
        master.httpApi.login(Some(UserAndPassword(UserId("UpdateRepoAgentTest"), SecretString("TEST-PASSWORD")))) await 99.seconds

        provider.runAgents() { _ =>
          runOrder(master, OrderId("🔺"))
        }

        // Start a new Agent with same state but a (probably) different HTTP port
        val port = findRandomFreeTcpPort()
        agent2 = RunningAgent.startForTest(AgentConfiguration.forTest(
          provider.agents.head.directory,
          httpPort = Some(port))
        ).await(99.seconds)

        val agentRef = AgentRef(agentRefPath % V1, uri = agent2.localUri.toString)
        executeCommand(master, UpdateRepo(V1, provider.fileBasedSigner.sign(agentRef) :: Nil))

        runOrder(master, OrderId("🔵"))
      }

      // Master recovery
      provider.runMaster() { master =>
        runOrder(master, OrderId("⭕"))
      }

      agent2.terminate() await 99.seconds
    }
  }


  private def executeCommand(master: RunningMaster, cmd: MasterCommand): Checked[cmd.Response] =
    master.httpApi.executeCommand(cmd).map(Valid.apply)
      .onErrorRecover { case e: HttpException if e.problem.isDefined ⇒ Invalid(e.problem.get) }
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
  private val V1 = VersionId("1")

  private def runOrder(master: RunningMaster, orderId: OrderId): Unit = {
    master.addOrderBlocking(FreshOrder(orderId, workflow.path))
    master.eventWatch.await[OrderFinished](predicate = _.key == orderId)
  }
}
