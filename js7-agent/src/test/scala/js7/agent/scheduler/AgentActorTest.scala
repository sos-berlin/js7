package js7.agent.scheduler

import akka.pattern.ask
import akka.util.Timeout
import js7.agent.data.Problems.AgentDuplicateOrder
import js7.agent.data.commands.AgentCommand
import js7.agent.data.commands.AgentCommand.{AttachOrder, AttachSignedItem, CoupleController, CreateAgent, DetachOrder, GetOrders}
import js7.agent.scheduler.AgentActorTest._
import js7.agent.scheduler.order.TestAgentActorProvider
import js7.base.io.file.FileUtils.syntax._
import js7.base.problem.Checked
import js7.base.problem.Checked.Ops
import js7.base.system.OperatingSystem.isWindows
import js7.base.thread.Futures.implicits._
import js7.base.thread.MonixBlocking.syntax._
import js7.base.time.ScalaTime._
import js7.base.time.Stopwatch
import js7.base.time.WaitForCondition.waitForCondition
import js7.base.utils.ScalaUtils.syntax._
import js7.data.agent.AgentPath
import js7.data.controller.ControllerId
import js7.data.event.{EventId, EventRequest}
import js7.data.order.{HistoricOutcome, Order, OrderEvent, OrderId, Outcome}
import js7.data.value.{NumberValue, StringValue}
import js7.data.workflow.position.Position
import js7.data.workflow.test.TestSetting._
import js7.journal.watch.EventWatch
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class AgentActorTest extends AnyFreeSpec
{
  private implicit val askTimeout = Timeout(60.s)

  for (n <- List(10) ++ (sys.props.contains("test.speed") ? 1000 /*needs taskLimit=100 !!!*/)) {
    s"AgentActorTest, $n orders" in {
      TestAgentActorProvider.provide { provider =>
        import provider.{agentDirectory, executeCommand}
        for (pathExecutable <- TestPathExecutables) {
          val file = pathExecutable.toFile(agentDirectory / "config" / "executables")
          file.writeExecutable(TestScript)
        }
        (provider.agentActor ? AgentActor.Input.Start).mapTo[AgentActor.Output.Ready.type] await 99.s
        val agentRunId = executeCommand(CreateAgent(controllerId, agentPath))
          .await(99.s).orThrow.asInstanceOf[CreateAgent.Response].agentRunId
        val eventWatch = (provider.agentActor ? AgentActor.Input.GetEventWatch)(Timeout(88.s))
          .mapTo[Checked[EventWatch]].await(99.s).orThrow
        val stopwatch = new Stopwatch
        val orderIds = for (i <- 0 until n) yield OrderId(s"TEST-ORDER-$i")
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

        waitForCondition(10.s, 10.ms) {
          // orderRegister is updated lately, so we may wait a moment
          val Right(GetOrders.Response(orders)) = executeCommand(GetOrders) await 99.s
          !orders.exists(_.isAttached/*should be _.isDetaching*/)
        }
        val Right(GetOrders.Response(orders)) = executeCommand(GetOrders) await 99.s
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
