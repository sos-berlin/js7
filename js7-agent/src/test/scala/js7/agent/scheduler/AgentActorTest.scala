package js7.agent.scheduler

import js7.agent.data.AgentState
import js7.agent.data.commands.AgentCommand
import js7.agent.data.commands.AgentCommand.{AttachItem, AttachOrder, AttachSignedItem, CoupleController, DedicateAgentDirector, DetachOrder}
import js7.agent.scheduler.AgentActorTest.*
import js7.agent.scheduler.order.TestAgentActorProvider
import js7.base.io.file.FileUtils.syntax.*
import js7.base.problem.Checked.Ops
import js7.base.system.OperatingSystem.isWindows
import js7.base.test.OurTestSuite
import js7.base.thread.Futures.implicits.*
import js7.base.thread.MonixBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.time.Stopwatch
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.web.Uri
import js7.data.agent.Problems.AgentDuplicateOrder
import js7.data.agent.{AgentPath, AgentRef}
import js7.data.controller.ControllerId
import js7.data.event.{EventId, EventRequest}
import js7.data.order.{HistoricOutcome, Order, OrderEvent, OrderId, Outcome}
import js7.data.subagent.{SubagentId, SubagentItem}
import js7.data.value.{NumberValue, StringValue}
import js7.data.workflow.position.Position
import js7.data.workflow.test.TestSetting.*
import js7.journal.recover.Recovered
import js7.tester.ScalaTestUtils.awaitAndAssert
import monix.execution.Scheduler.Implicits.traced
import org.apache.pekko.pattern.ask
import org.apache.pekko.util.Timeout
import scala.concurrent.duration.Deadline.now

/**
  * @author Joacim Zschimmer
  */
final class AgentActorTest extends OurTestSuite
{
  private implicit val askTimeout: Timeout = Timeout(60.s)

  for (n <- List(10) ++ (sys.props.contains("test.speed") ? 1000 /*needs Job.processLimit=100 !!!*/)) {
    s"AgentActorTest, $n orders" in {
      TestAgentActorProvider.provide("AgentActorTest") { provider =>
        import provider.{agentDirectory, executeCommand}
        val agentConfiguration = provider.agentConfiguration
        val persistence = provider.persistence
        import agentConfiguration.{config, journalMeta}
        import persistence.eventWatch

        for (pathExecutable <- TestPathExecutables) {
          val file = pathExecutable.toFile(agentDirectory / "config" / "executables")
          file.writeUtf8Executable(TestScript)
        }

        (provider.agentActor ?
          AgentActor.Input.Start(Recovered.noJournalFile[AgentState](journalMeta, now, config))
        ).mapTo[AgentActor.Output.Ready.type] await 99.s


        val subagentId = SubagentId("SUBAGENT")
        val agentRunId = executeCommand(
          DedicateAgentDirector(Some(subagentId), controllerId, agentPath)
        ).await(99.s).orThrow.asInstanceOf[DedicateAgentDirector.Response].agentRunId

        executeCommand(AttachItem(AgentRef(agentPath, Seq(subagentId)))).await(99.s).orThrow

        val stopwatch = new Stopwatch
        val orderIds = for (i <- 0 until n) yield OrderId(s"TEST-ORDER-$i")

        executeCommand(AttachItem(SubagentItem(subagentId, agentPath, Uri("https://0.0.0.0:0"))))
          .await(99.s).orThrow
        executeCommand(AttachSignedItem(provider.itemSigner.sign(SimpleTestWorkflow)))
          .await(99.s).orThrow

        orderIds.map(orderId =>
          executeCommand(
            AttachOrder(TestOrder.copy(id = orderId), TestAgentPath))
        ).await(99.s).foreach(o => assert(o.isRight))

        assert(
          executeCommand(
            AttachOrder(TestOrder.copy(id = orderIds.head), TestAgentPath)
          ).await(99.s) == Left(AgentDuplicateOrder(orderIds.head)))

        assert(executeCommand(CoupleController(agentPath, agentRunId, EventId.BeforeFirst)).await(99.s) ==
          Right(CoupleController.Response(orderIds.toSet)))

        for (orderId <- orderIds)
          eventWatch.whenKeyedEvent[OrderEvent.OrderDetachable](EventRequest.singleClass(timeout = Some(90.s)), orderId) await 99.s
        info(stopwatch.itemsPerSecondString(n, "Orders"))

        awaitAndAssert {
          // orderRegister is updated lately, so we may wait a moment
          !persistence.currentState.idToOrder.values.exists(_.isAttached/*should be _.isDetaching*/)
        }
        val orders = persistence.currentState.idToOrder.values

        assert(orders.toSet ==
          orderIds.map(orderId => Order(
            orderId,
            SimpleTestWorkflow.lastWorkflowPosition,
            Order.Ready,
            Map("KEY" -> StringValue("VALUE")),
            historicOutcomes = TestOrder.historicOutcomes :+
              HistoricOutcome(Position(0), Outcome.Succeeded(Map("returnCode" -> NumberValue(0), "result" -> StringValue("TEST-RESULT-")))) :+
              HistoricOutcome(Position(1), Outcome.Succeeded(Map("returnCode" -> NumberValue(0), "result" -> StringValue("TEST-RESULT-B-VALUE")))),
            attachedState = Some(Order.Detaching(TestAgentPath))
          )).toSet)

        (for (orderId <- orderIds) yield executeCommand(DetachOrder(orderId))) await 99.s
        for (orderId <- orderIds)
          eventWatch.whenKeyedEvent[OrderEvent.OrderDetached](EventRequest.singleClass(timeout = Some(90.s)), orderId) await 99.s
        executeCommand(AgentCommand.ShutDown()) await 99.s
      }
    }
  }
}

object AgentActorTest
{
  private val agentPath = AgentPath("AGENT")
  private val controllerId = ControllerId("CONTROLLER")
  private val TestScript =
    if (isWindows) """
      |@echo off
      |echo Hej!
      |echo result=TEST-RESULT-%SCHEDULER_PARAM_JOB_B% >>"%SCHEDULER_RETURN_VALUES%"
      |""".stripMargin
    else """
      |echo "Hej!"
      |echo "result=TEST-RESULT-$SCHEDULER_PARAM_JOB_B" >>"$SCHEDULER_RETURN_VALUES"
      |""".stripMargin
}
