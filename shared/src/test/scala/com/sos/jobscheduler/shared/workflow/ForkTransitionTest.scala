package com.sos.jobscheduler.shared.workflow

import com.sos.jobscheduler.base.utils.MapDiff
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.event.KeyedEvent
import com.sos.jobscheduler.data.order.{Order, OrderEvent, OrderId, Payload}
import com.sos.jobscheduler.data.workflow.Workflow.JobNode
import com.sos.jobscheduler.data.workflow.transition.Transition
import com.sos.jobscheduler.data.workflow.transitions.{ForkTransition, JoinTransition}
import com.sos.jobscheduler.data.workflow.{JobPath, NodeId, NodeKey, Workflow, WorkflowPath}
import com.sos.jobscheduler.shared.workflow.ForkTransitionTest._
import com.sos.jobscheduler.shared.workflow.Transitions.ExecutableTransition
import org.scalatest.FreeSpec


/**
  * @author Joacim Zschimmer
  */
final class ForkTransitionTest extends FreeSpec {

  "fork" in {
    val order = TestOrder.copy(state = Order.Processed)
    val nodeToOrder = Map(A.id → order)
    assert(fork.switch(nodeToOrder) == Some(
      KeyedEvent(
        OrderEvent.OrderForked(List(
          OrderEvent.OrderForked.Child(BxOrderId, Bx.id, order.payload),
          OrderEvent.OrderForked.Child(ByOrderId, By.id, order.payload)))
      )(order.id)))
  }

  "join" in {
    val nodeToOrder = Map(
      A.id → TestOrder.copy(id = TestOrder.id, state = Order.Forked(Vector(BxOrderId, ByOrderId))),
      Bx.id → TestOrder.copy(id = BxOrderId, state = Order.Processed),
      By.id → TestOrder.copy(id = ByOrderId, state = Order.Processed))
    assert(join.switch(nodeToOrder) == Some(
      KeyedEvent(
        OrderEvent.OrderJoined(C.id, MapDiff.empty, TestOrder.outcome)
      )(TestOrder.id)))
  }
}

object ForkTransitionTest {
  private val TestAgentPath = AgentPath("/AGENT")
  private val TestJobPath = JobPath("/JOB")

  private val A = JobNode(NodeId("A"), TestAgentPath, TestJobPath)
  private val Bx = JobNode(NodeId("Bx"), TestAgentPath, TestJobPath)
  private val By = JobNode(NodeId("By"), TestAgentPath, TestJobPath)
  private val C = JobNode(NodeId("C"), TestAgentPath, TestJobPath)

  private val fork = Transition(from = Vector(A), Vector(Bx, By), ForkTransition)
  private val join = Transition.join(fromForked = A, fromProcessed = Vector(Bx, By), to = Vector(C), JoinTransition)

  private val TestWorkflow = Workflow(WorkflowPath("/WORKFLOW"), A.id, List(fork, join))

  private val TestOrder = Order(OrderId("TEST"), NodeKey(TestWorkflow.path, A.id), Order.Ready, Some(Order.AttachedTo.Agent(TestAgentPath)),
    Payload(Map("VARIABLE" → "VALUE")))
  private val BxOrderId = OrderId("TEST/Bx")
  private val ByOrderId = OrderId("TEST/By")
}
