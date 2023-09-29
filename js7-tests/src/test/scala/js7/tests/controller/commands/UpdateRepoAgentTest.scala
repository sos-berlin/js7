package js7.tests.controller.commands

import js7.agent.TestAgent
import js7.agent.configuration.AgentConfiguration
import js7.base.io.file.FileUtils.syntax.*
import js7.base.problem.Checked.Ops
import js7.base.test.OurTestSuite
import js7.base.thread.MonixBlocking.syntax.RichTask
import js7.base.time.ScalaTime.*
import js7.base.utils.AutoClosing.autoClosing
import js7.common.utils.FreeTcpPortFinder.findFreeTcpPort
import js7.data.agent.AgentPath
import js7.data.job.RelativePathExecutable
import js7.data.order.OrderEvent.OrderFinished
import js7.data.order.{FreshOrder, OrderId}
import js7.data.workflow.{WorkflowParser, WorkflowPath}
import js7.tests.controller.commands.UpdateRepoAgentTest.*
import js7.tests.testenv.{DirectoryProvider, TestController}
import monix.execution.Scheduler.Implicits.traced

/**
  * @author Joacim Zschimmer
  */
final class UpdateRepoAgentTest extends OurTestSuite:
  "ControllerCommand.UpdateRepo" in:
    val directoryProvider = new DirectoryProvider(
      agentPaths = agentPath :: Nil,
      items = workflow :: Nil,
      testName = Some("UpdateRepoAgentTest"))
    autoClosing(directoryProvider) { _ =>
      directoryProvider.controllerEnv.privateConf ++=
        """js7.auth.users {
          |  TEST-USER {
          |    password = "plain:TEST-PASSWORD"
          |    permissions = [ UpdateItem ]
          |  }
          |}
          |""".stripMargin
      directoryProvider.agentToEnv(agentPath)
        .writeExecutable(RelativePathExecutable("SCRIPT.cmd"), ":")

      // Start Agent before Controller to bind the reserved TCP port early, and the Controller needs not to wait
      val agent1 = directoryProvider.startAgents().await(99.s).head
      var agent2: TestAgent = null
      directoryProvider.runController() { controller =>
        runOrder(controller, OrderId("🔺"))
        agent1.terminate() await 99.s

        for i <- 1 to 3 do
          if agent2 != null then agent2.terminate() await 99.s
          // Start a new Agent with same state but a (hopefully) different HTTP port
          val port = findFreeTcpPort()
          agent2 = TestAgent.start(AgentConfiguration.forTest(
            directoryProvider.agentEnvs.head.directory,
            name = "UpdateRepoAgentTest",
            httpPort = Some(port))
          ).await(99.s)

          controller.api
            .updateUnsignedSimpleItems(Seq(
              directoryProvider.subagentItems.head.copy(uri = agent2.localUri)))
            .await(99.s).orThrow
          runOrder(controller, OrderId(s"♣️-$i"))
      }

      // Controller recovery
      directoryProvider.runController() { controller =>
        runOrder(controller, OrderId("♠️"))
      }

      agent2.terminate() await 99.s
    }

object UpdateRepoAgentTest:
  private val agentPath = AgentPath("AGENT")
  private val workflow = WorkflowParser.parse(WorkflowPath("WORKFLOW"),
     """define workflow {
          execute executable="SCRIPT.cmd", agent="AGENT";
        }"""
  ).orThrow

  private def runOrder(controller: TestController, orderId: OrderId): Unit =
    controller.addOrderBlocking(FreshOrder(orderId, workflow.path))
    controller.eventWatch.await[OrderFinished](predicate = _.key == orderId)
