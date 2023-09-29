package js7.data.execution.workflow.instructions

import java.time.DayOfWeek.FRIDAY
import java.time.{LocalDate, LocalTime}
import js7.base.problem.Checked.*
import js7.base.test.OurTestSuite
import js7.base.time.ScalaTime.*
import js7.base.time.{AdmissionTimeScheme, WallClock, WeekdayPeriod}
import js7.data.agent.AgentPath
import js7.data.execution.workflow.instructions.ExecuteExecutor.orderIdToDate
import js7.data.execution.workflow.instructions.ExecuteExecutorTest.*
import js7.data.job.InternalExecutable
import js7.data.order.OrderEvent.{OrderAttachable, OrderMoved}
import js7.data.order.{Order, OrderId}
import js7.data.state.TestStateView
import js7.data.workflow.instructions.Execute
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.position.Position
import js7.data.workflow.{Workflow, WorkflowPath}
import scala.collection.View

/**
  * @author Joacim Zschimmer
  */
final class ExecuteExecutorTest extends OurTestSuite
{
  private lazy val stateView = TestStateView.of(
    isAgent = false,
    orders = Some(orders),
    workflows = Some(Seq(workflow)))
  private lazy val executorService = new InstructionExecutorService(WallClock)

  "orderIdToDate" in {
    assert(orderIdToDate(OrderId("x")) == None)
    assert(orderIdToDate(OrderId("#no-date#")) == None)
    assert(orderIdToDate(OrderId("#2021-09-03")) == None)
    assert(orderIdToDate(OrderId("#2021-02-29#")) == None)
    assert(orderIdToDate(OrderId("#2021-13-13#")) == None)
    assert(orderIdToDate(OrderId("#2021-09-03#")) == Some(LocalDate.parse("2021-09-03")))
    assert(orderIdToDate(OrderId("#2021-09-03#xx")) == Some(LocalDate.parse("2021-09-03")))
  }

  "No or non-skipped AdmissionTimeScheme" in {
    for position <- View(Position(0)/*no scheme*/, Position(1)/*not skipped*/) do {
      val execute = workflow.instruction_[Execute](position).orThrow
      for order <- orders.filter(_.position == position) do {
        assert(executorService.toEvents(execute, order, stateView) ==
          Right((order.id <-: OrderAttachable(agentPath)) :: Nil))
        assert(executorService.nextMove(execute, order, stateView) ==
          Right(None))
      }
    }
  }

  "skipIfNoAdmissionStartForOrderDay" - {
    val position = Position(2)
    val execute = workflow.instruction_[Execute](position).orThrow

    "OrderId date has admission time" in {
      for orderId <- View(OrderId("#2021-09-03#2-Fresh"), OrderId("#2021-09-03#2-Ready")) do {
        val order = stateView.idToOrder(orderId)
        assert(executorService.toEvents(execute, order, stateView) ==
          Right((order.id <-: OrderAttachable(agentPath)) :: Nil))
        assert(executorService.nextMove(execute, order, stateView) ==
          Right(None))
      }
    }

    "OrderId date has no admission time" in {
      for (orderId <- View(
        OrderId("#2021-09-02#2-Fresh"),
        OrderId("#2021-09-02#2-Ready"),
        OrderId("#2021-09-04#2-Ready"))) {
        val order = stateView.idToOrder(orderId)
        assert(executorService.toEvents(execute, order, stateView) ==
          Right(
            (order.id <-: OrderMoved(position.increment, Some(OrderMoved.NoAdmissionPeriodStart)))
              :: Nil))
        assert(executorService.nextMove(execute, order, stateView) ==
          Right(Some(
            OrderMoved(position.increment, reason = Some(OrderMoved.NoAdmissionPeriodStart)))))
      }
    }
  }
}

object ExecuteExecutorTest
{
  private val admissionTimeScheme = AdmissionTimeScheme(Seq(
    WeekdayPeriod(FRIDAY, LocalTime.of(8, 0), 25.h)))
  private val agentPath = AgentPath("AGENT")
  private val workflow = Workflow(WorkflowPath("WORKFLOW") ~ "VERSION", Seq(
    /*0*/Execute(WorkflowJob(agentPath, InternalExecutable("?"))),
    /*1*/Execute(WorkflowJob(
      agentPath,
      InternalExecutable("?"),
      admissionTimeScheme = Some(admissionTimeScheme))),
    /*2*/Execute(WorkflowJob(
      agentPath,
      InternalExecutable("?"),
      admissionTimeScheme = Some(admissionTimeScheme),
      skipIfNoAdmissionStartForOrderDay = true))))

  private val orders = Seq(
    Order(OrderId("#2021-09-03#0-Fresh"), workflow.id /: Position(0), Order.Fresh),
    Order(OrderId("#2021-09-03#0-Ready"), workflow.id /: Position(0), Order.Ready),
    Order(OrderId("#2021-09-02#0-Fresh"), workflow.id /: Position(0), Order.Fresh),
    Order(OrderId("#2021-09-02#0-Ready"), workflow.id /: Position(0), Order.Ready),

    Order(OrderId("#2021-09-03#1-Fresh"), workflow.id /: Position(1), Order.Fresh),
    Order(OrderId("#2021-09-03#1-Ready"), workflow.id /: Position(1), Order.Ready),
    Order(OrderId("#2021-09-02#1-Fresh"), workflow.id /: Position(1), Order.Fresh),
    Order(OrderId("#2021-09-02#1-Ready"), workflow.id /: Position(1), Order.Ready),

    Order(OrderId("#2021-09-03#2-Fresh"), workflow.id /: Position(2), Order.Fresh),
    Order(OrderId("#2021-09-03#2-Ready"), workflow.id /: Position(2), Order.Ready),
    Order(OrderId("#2021-09-02#2-Fresh"), workflow.id /: Position(2), Order.Fresh),
    Order(OrderId("#2021-09-02#2-Ready"), workflow.id /: Position(2), Order.Ready),

    Order(OrderId("#2021-09-04#2-Ready"), workflow.id /: Position(2), Order.Ready))
}
