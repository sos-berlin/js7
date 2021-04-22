package js7.tests.controller.commands

import js7.agent.RunningAgent
import js7.agent.configuration.AgentConfiguration
import js7.base.auth.{UserAndPassword, UserId}
import js7.base.generic.SecretString
import js7.base.io.file.FileUtils.syntax._
import js7.base.log.ScribeUtils.coupleScribeWithSlf4j
import js7.base.problem.Checked
import js7.base.problem.Checked.Ops
import js7.base.thread.Futures.implicits._
import js7.base.thread.MonixBlocking.syntax.RichTask
import js7.base.time.ScalaTime._
import js7.base.utils.AutoClosing.autoClosing
import js7.base.web.HttpClient.HttpException
import js7.common.utils.FreeTcpPortFinder.findFreeTcpPort
import js7.controller.RunningController
import js7.data.agent.{AgentPath, AgentRef}
import js7.data.controller.ControllerCommand
import js7.data.job.RelativePathExecutable
import js7.data.order.OrderEvent.OrderFinished
import js7.data.order.{FreshOrder, OrderId}
import js7.data.workflow.{WorkflowParser, WorkflowPath}
import js7.tests.controller.commands.UpdateRepoAgentTest._
import js7.tests.testenv.DirectoryProvider
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AnyFreeSpec
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
final class UpdateRepoAgentTest extends AnyFreeSpec
{
  coupleScribeWithSlf4j()

  "ControllerCommand.UpdateRepo" in {
    autoClosing(new DirectoryProvider(agentId :: Nil, workflow :: Nil, testName = Some("UpdateRepoAgentTest"))) { provider =>
      (provider.controller.configDir / "private" / "private.conf") ++=
        """js7.auth.users {
          |  UpdateRepoAgentTest {
          |    password = "plain:TEST-PASSWORD"
          |    permissions = [ UpdateItem ]
          |  }
          |}
          |""".stripMargin
      provider.agentToTree(agentId).writeExecutable(RelativePathExecutable("SCRIPT.cmd"), ":")

      // Start Agent before Controller to bind the reserved TCP port early, and the Controller needs not to wait
      val agent1 = provider.startAgents().await(99.s).head
      var agent2: RunningAgent = null
      provider.runController() { controller =>
        controller.httpApi.login_(Some(UserAndPassword(UserId("UpdateRepoAgentTest"), SecretString("TEST-PASSWORD")))) await 99.s
        runOrder(controller, OrderId("🔺"))
        agent1.terminate() await 99.s

        for (i <- 1 to 3) {
          if (agent2 != null) agent2.terminate() await 99.s
          // Start a new Agent with same state but a (hopefully) different HTTP port
          val port = findFreeTcpPort()
          agent2 = RunningAgent.startForTest(AgentConfiguration.forTest(
            provider.agents.head.directory,
            httpPort = Some(port))
          ).await(99.s)

          controller.updateUnsignedSimpleItemsAsSystemUser(Seq(AgentRef(agentId, uri = agent2.localUri))).await(99.s).orThrow
          runOrder(controller, OrderId(s"🔵-$i"))
        }
      }

      // Controller recovery
      provider.runController() { controller =>
        runOrder(controller, OrderId("⭕"))
      }

      agent2.terminate() await 99.s
    }
  }

  private def executeCommand(controller: RunningController, cmd: ControllerCommand): Checked[cmd.Response] =
    controller.httpApi.executeCommand(cmd).map(Right.apply)
      .onErrorRecover { case HttpException.HasProblem(problem) => Left(problem) }
      .await(99.seconds)
}

object UpdateRepoAgentTest
{
  private val agentId = AgentPath("AGENT")
  private val workflow = WorkflowParser.parse(WorkflowPath("WORKFLOW"),
     """define workflow {
          execute executable="SCRIPT.cmd", agent="AGENT";
        }"""
  ).orThrow

  private def runOrder(controller: RunningController, orderId: OrderId): Unit = {
    controller.addOrderBlocking(FreshOrder(orderId, workflow.path))
    controller.eventWatch.await[OrderFinished](predicate = _.key == orderId)
  }
}
