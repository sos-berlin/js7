package com.sos.jobscheduler.core.workflow.instructions

import com.sos.jobscheduler.core.workflow.instructions.TryExecutorTest._
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.job.{ExecutablePath, ReturnCode}
import com.sos.jobscheduler.data.order.OrderEvent.OrderMoved
import com.sos.jobscheduler.data.order.{Order, OrderId, Outcome}
import com.sos.jobscheduler.data.workflow.instructions.executable.WorkflowJob
import com.sos.jobscheduler.data.workflow.instructions.{Execute, TryInstruction}
import com.sos.jobscheduler.data.workflow.position.{Position, WorkflowPosition}
import com.sos.jobscheduler.data.workflow.{OrderContext, Workflow, WorkflowPath}
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class TryExecutorTest extends FreeSpec
{
  private lazy val context = new OrderContext {
    def idToOrder = Map(AOrder.id → AOrder)
    def childOrderEnded(order: Order[Order.State]) = throw new NotImplementedError
    def instruction(position: WorkflowPosition) = throw new NotImplementedError
  }


  "nextPosition" in {
    assert(InstructionExecutor.nextPosition(context, AOrder, tryInstruction) ==
      Some(Position(7, 0, 0)))
  }

  "toEvent" in {
    assert(InstructionExecutor.toEvent(tryInstruction, AOrder, context) ==
      Some(AOrder.id  <-: OrderMoved(Position(7, 0, 0))))
  }
}

object TryExecutorTest {
  private val TestWorkflowId = WorkflowPath("/WORKFLOW") % "VERSION"
  private val AOrder = Order(OrderId("ORDER-A"), TestWorkflowId /: Position(7), Order.Fresh(), Outcome.Succeeded(ReturnCode(1)))
  private val TryJob = Execute(WorkflowJob(AgentPath("/AGENT"), ExecutablePath("/THEN")))
  private val CatchJob = Execute(WorkflowJob(AgentPath("/AGENT"), ExecutablePath("/ELSE")))
  private val tryInstruction = TryInstruction(Workflow.of(TryJob), Workflow.of(CatchJob))
}
