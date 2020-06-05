package js7.tests.master.commands

import js7.agent.RunningAgent
import js7.agent.configuration.AgentConfiguration
import js7.base.auth.{UserAndPassword, UserId}
import js7.base.generic.SecretString
import js7.base.problem.Checked
import js7.base.problem.Checked.Ops
import js7.base.utils.AutoClosing.autoClosing
import js7.common.http.AkkaHttpClient.HttpException
import js7.common.log.ScribeUtils
import js7.common.scalautil.FileUtils.syntax._
import js7.common.scalautil.Futures.implicits._
import js7.common.scalautil.MonixUtils.syntax.RichTask
import js7.common.utils.FreeTcpPortFinder.findFreeTcpPort
import js7.data.agent.{AgentRef, AgentRefPath}
import js7.data.filebased.VersionId
import js7.data.job.ExecutablePath
import js7.data.order.OrderEvent.OrderFinished
import js7.data.order.{FreshOrder, OrderId}
import js7.data.workflow.WorkflowPath
import js7.data.workflow.parser.WorkflowParser
import js7.master.RunningMaster
import js7.master.data.MasterCommand
import js7.master.data.MasterCommand.UpdateRepo
import js7.tests.master.commands.UpdateRepoAgentTest._
import js7.tests.testenv.DirectoryProvider
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AnyFreeSpec
import scala.concurrent.duration._

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
        master.httpApi.login_(Some(UserAndPassword(UserId("UpdateRepoAgentTest"), SecretString("TEST-PASSWORD")))) await 99.seconds
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
