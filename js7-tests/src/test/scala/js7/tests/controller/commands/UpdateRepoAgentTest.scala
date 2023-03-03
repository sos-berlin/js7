package js7.tests.controller.commands

import js7.agent.RunningAgent
import js7.agent.configuration.AgentConfiguration
import js7.base.auth.{UserAndPassword, UserId}
import js7.base.generic.SecretString
import js7.base.io.file.FileUtils.syntax.*
import js7.base.log.ScribeForJava.coupleScribeWithSlf4j
import js7.base.problem.Checked
import js7.base.problem.Checked.Ops
import js7.base.test.OurTestSuite
import js7.base.thread.Futures.implicits.*
import js7.base.thread.MonixBlocking.syntax.RichTask
import js7.base.time.ScalaTime.*
import js7.base.utils.AutoClosing.autoClosing
import js7.base.web.HttpClient.HttpException
import js7.common.utils.FreeTcpPortFinder.findFreeTcpPort
import js7.data.agent.AgentPath
import js7.data.controller.ControllerCommand
import js7.data.job.RelativePathExecutable
import js7.data.order.OrderEvent.OrderFinished
import js7.data.order.{FreshOrder, OrderId}
import js7.data.workflow.{WorkflowParser, WorkflowPath}
import js7.tests.controller.commands.UpdateRepoAgentTest.*
import js7.tests.testenv.{DirectoryProvider, TestController}
import monix.execution.Scheduler.Implicits.traced
import scala.concurrent.duration.*

/**
  * @author Joacim Zschimmer
  */
final class UpdateRepoAgentTest extends OurTestSuite
{
  coupleScribeWithSlf4j()

  "ControllerCommand.UpdateRepo" in {
    val directoryProvider = new DirectoryProvider(
      agentPaths = agentPath :: Nil,
      items = workflow :: Nil,
      testName = Some("UpdateRepoAgentTest"))
    autoClosing(directoryProvider) { _ =>
      (directoryProvider.controller.configDir / "private" / "private.conf") ++=
        """js7.auth.users {
          |  UpdateRepoAgentTest {
          |    password = "plain:TEST-PASSWORD"
          |    permissions = [ UpdateItem ]
          |  }
          |}
          |""".stripMargin
      directoryProvider.agentToTree(agentPath)
        .writeExecutable(RelativePathExecutable("SCRIPT.cmd"), ":")

      // Start Agent before Controller to bind the reserved TCP port early, and the Controller needs not to wait
      val agent1 = directoryProvider.startAgents().await(99.s).head
      var agent2: RunningAgent = null
      directoryProvider.runController() { controller =>
        controller.httpApi
          .login_(Some(UserAndPassword(
            UserId("UpdateRepoAgentTest"),
            SecretString("TEST-PASSWORD"))))
          .await(99.s)
        runOrder(controller, OrderId("🔺"))
        agent1.terminate() await 99.s

        for (i <- 1 to 3) {
          if (agent2 != null) agent2.terminate() await 99.s
          // Start a new Agent with same state but a (hopefully) different HTTP port
          val port = findFreeTcpPort()
          agent2 = RunningAgent.startForTest(AgentConfiguration.forTest(
            directoryProvider.agents.head.directory,
            name = "UpdateRepoAgentTest",
            httpPort = Some(port))
          ).await(99.s)

          controller.updateUnsignedSimpleItemsAsSystemUser(Seq(
            directoryProvider.subagentItems.head.copy(uri = agent2.localUri)
          )).await(99.s).orThrow
          runOrder(controller, OrderId(s"🔵-$i"))
        }
      }

      // Controller recovery
      directoryProvider.runController() { controller =>
        runOrder(controller, OrderId("⭕"))
      }

      agent2.terminate() await 99.s
    }
  }

  private def executeCommand(controller: TestController, cmd: ControllerCommand): Checked[cmd.Response] =
    controller.httpApi.executeCommand(cmd).map(Right.apply)
      .onErrorRecover { case HttpException.HasProblem(problem) => Left(problem) }
      .await(99.seconds)
}

object UpdateRepoAgentTest
{
  private val agentPath = AgentPath("AGENT")
  private val workflow = WorkflowParser.parse(WorkflowPath("WORKFLOW"),
     """define workflow {
          execute executable="SCRIPT.cmd", agent="AGENT";
        }"""
  ).orThrow

  private def runOrder(controller: TestController, orderId: OrderId): Unit = {
    controller.addOrderBlocking(FreshOrder(orderId, workflow.path))
    controller.eventWatch.await[OrderFinished](predicate = _.key == orderId)
  }
}
