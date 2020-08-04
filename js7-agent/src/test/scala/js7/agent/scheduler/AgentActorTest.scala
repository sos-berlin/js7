package js7.agent.scheduler

import akka.pattern.ask
import akka.util.Timeout
import js7.agent.data.Problems.AgentDuplicateOrder
import js7.agent.data.commands.AgentCommand
import js7.agent.data.commands.AgentCommand.{AttachOrder, CoupleController, DetachOrder, GetOrders, RegisterAsController}
import js7.agent.scheduler.AgentActorTest._
import js7.agent.scheduler.order.TestAgentActorProvider
import js7.base.problem.Checked.Ops
import js7.base.time.ScalaTime._
import js7.base.time.Stopwatch
import js7.base.utils.ScalaUtils.syntax._
import js7.common.scalautil.FileUtils.syntax._
import js7.common.scalautil.Futures.implicits._
import js7.common.scalautil.MonixUtils.syntax._
import js7.common.system.OperatingSystem.isWindows
import js7.data.agent.AgentRefPath
import js7.data.event.{EventId, EventRequest}
import js7.data.order.{HistoricOutcome, Order, OrderEvent, OrderId, Outcome}
import js7.data.workflow.position.Position
import js7.data.workflow.test.TestSetting._
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class AgentActorTest extends AnyFreeSpec
{
  private implicit val askTimeout = Timeout(60.s)

  for (n <- List(10) ++ (sys.props contains "test.speed" option 1000 /*needs taskLimit=100 !!!*/)) {
    s"AgentActorTest, $n orders" in {
      TestAgentActorProvider.provide { provider =>
        import provider.{agentDirectory, eventCollector, executeCommand}
        for (executablePath <- TestExecutablePaths) {
          val file = executablePath.toFile(agentDirectory / "config" / "executables")
          file.writeExecutable(TestScript)
        }
        (provider.agentActor ? AgentActor.Input.Start).mapTo[AgentActor.Output.Ready.type] await 99.s
        val agentRunId = executeCommand(RegisterAsController(agentRefPath))
          .await(99.s).orThrow.asInstanceOf[RegisterAsController.Response].agentRunId
        val stopwatch = new Stopwatch
        val orderIds = for (i <- 0 until n) yield OrderId(s"TEST-ORDER-$i")
        orderIds.map(orderId =>
          executeCommand(
            AttachOrder(TestOrder.copy(id = orderId), TestAgentRefPath, provider.itemSigner.sign(SimpleTestWorkflow)))
        ).await(99.s).foreach(o => assert(o.isRight))
        assert(
          executeCommand(
            AttachOrder(TestOrder.copy(id = orderIds.head), TestAgentRefPath, provider.itemSigner.sign(SimpleTestWorkflow))
          ).await(99.s) == Left(AgentDuplicateOrder(orderIds.head)))
        assert(executeCommand(CoupleController(agentRefPath, agentRunId, EventId.BeforeFirst)).await(99.s) ==
          Right(CoupleController.Response(orderIds.toSet)))
        for (orderId <- orderIds)
          eventCollector.whenKeyedEvent[OrderEvent.OrderDetachable](EventRequest.singleClass(timeout = Some(90.s)), orderId) await 99.s
        info(stopwatch.itemsPerSecondString(n, "Orders"))

        val Right(GetOrders.Response(orders)) = executeCommand(GetOrders) await 99.s
        assert(orders.toSet ==
          orderIds.map(orderId => Order(
            orderId,
            SimpleTestWorkflow.lastWorkflowPosition,
            Order.Ready,
            Map("KEY" -> "VALUE"),
            historicOutcomes = TestOrder.historicOutcomes :+
              HistoricOutcome(Position(0), Outcome.Succeeded(Map("result" -> "TEST-RESULT-"))) :+
              HistoricOutcome(Position(1), Outcome.Succeeded(Map("result" -> "TEST-RESULT-B-VALUE"))),
            Some(Order.Detaching(TestAgentRefPath))
          )).toSet)

        (for (orderId <- orderIds) yield executeCommand(DetachOrder(orderId))) await 99.s
        for (orderId <- orderIds)
          eventCollector.whenKeyedEvent[OrderEvent.OrderDetached](EventRequest.singleClass(timeout = Some(90.s)), orderId) await 99.s
        executeCommand(AgentCommand.ShutDown()) await 99.s
      }
    }
  }
}

object AgentActorTest
{
  private val agentRefPath = AgentRefPath("/AGENT")
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
