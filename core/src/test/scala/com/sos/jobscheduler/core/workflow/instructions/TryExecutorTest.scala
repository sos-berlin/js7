package com.sos.jobscheduler.core.workflow.instructions

import cats.data.Validated.Valid
import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.core.workflow.OrderContext
import com.sos.jobscheduler.core.workflow.instructions.TryExecutorTest._
import com.sos.jobscheduler.data.agent.AgentRefPath
import com.sos.jobscheduler.data.job.{ExecutablePath, ReturnCode}
import com.sos.jobscheduler.data.order.OrderEvent.OrderMoved
import com.sos.jobscheduler.data.order.{Order, OrderId, Outcome}
import com.sos.jobscheduler.data.workflow.instructions.executable.WorkflowJob
import com.sos.jobscheduler.data.workflow.instructions.{Execute, TryInstruction}
import com.sos.jobscheduler.data.workflow.position.BranchId.Try_
import com.sos.jobscheduler.data.workflow.position._
import com.sos.jobscheduler.data.workflow.{Workflow, WorkflowPath}
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class TryExecutorTest extends FreeSpec
{
  private lazy val context = new OrderContext {
    def idToOrder = Map(AOrder.id -> AOrder)
    def childOrderEnded(order: Order[Order.State]) = throw new NotImplementedError
    def instruction(position: WorkflowPosition) = throw new NotImplementedError
  }

  "JSON" - {
    "try" in {
      testJson(TryExecutor.nextPosition(context, AOrder, tryInstruction),
        json"""[ 7, "try", 0 ]""")
    }

    "catch" in {
      testJson(Position(7) / BranchId.Catch_ % 0,
        json"""[ 7, "catch", 0 ]""")
    }
  }

  "nextPosition" in {
    assert(InstructionExecutor.nextPosition(context, AOrder, tryInstruction) ==
      Some(Position(7) / Try_ % 0))
  }

  "toEvent" in {
    assert(InstructionExecutor.toEvent(tryInstruction, AOrder, context) ==
      Valid(Some(
        AOrder.id <-: OrderMoved(Position(7) / Try_ % 0))))
  }
}

object TryExecutorTest {
  private val TestWorkflowId = WorkflowPath("/WORKFLOW") ~ "VERSION"
  private val AOrder = Order(OrderId("ORDER-A"), TestWorkflowId /: Position(7), Order.Fresh(), Outcome.Succeeded(ReturnCode(1)))
  private val TryJob = Execute(WorkflowJob(AgentRefPath("/AGENT"), ExecutablePath("/THEN")))
  private val CatchJob = Execute(WorkflowJob(AgentRefPath("/AGENT"), ExecutablePath("/ELSE")))
  private val tryInstruction = TryInstruction(Workflow.of(TryJob), Workflow.of(CatchJob))
}
