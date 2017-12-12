package com.sos.jobscheduler.shared.workflow

import com.sos.jobscheduler.base.utils.MapDiff
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.event.KeyedEvent
import com.sos.jobscheduler.data.order.{Order, OrderEvent, OrderId, Payload}
import com.sos.jobscheduler.data.workflow.Workflow.JobNode
import com.sos.jobscheduler.data.workflow.transition.Transition
import com.sos.jobscheduler.data.workflow.transition.TransitionType.Outlet
import com.sos.jobscheduler.data.workflow.transitions.{ForkTransition, JoinTransition}
import com.sos.jobscheduler.data.workflow.{JobPath, NodeId, NodeKey, WorkflowPath}
import com.sos.jobscheduler.shared.workflow.ForkJoinTransitionTest._
import com.sos.jobscheduler.shared.workflow.Transitions.ExecutableTransition
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class ForkJoinTransitionTest extends FreeSpec {

  private val (fork, join) = Transition.forkJoin(
    forkNodeId = A.id,
    joinNodeId = C.id,
    outlets = Vector(
      Outlet(Outlet.Id("XXX"), Bx.id),
      Outlet(Outlet.Id("YYY"), By.id)),
    Vector(Bx.id, By.id),
    ForkTransition,
    JoinTransition)

  "Transktion.fork" in {
    val order = TestOrder.copy(state = Order.Processed)
    val nodeToOrder = Map(A.id → order)
    assert(fork.switch(nodeToOrder) == Some(
      KeyedEvent(
        OrderEvent.OrderForked(List(
          OrderEvent.OrderForked.Child(OrderId("TEST/XXX"), Bx.id, order.payload),
          OrderEvent.OrderForked.Child(OrderId("TEST/YYY"), By.id, order.payload)))
      )(order.id)))
  }

  "Transition.join" in {
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

object ForkJoinTransitionTest {
  private val TestAgentPath = AgentPath("/AGENT")
  private val TestJobPath = JobPath("/JOB")

  private val A = JobNode(NodeId("A"), TestAgentPath, TestJobPath)
  private val Bx = JobNode(NodeId("Bx"), TestAgentPath, TestJobPath)
  private val By = JobNode(NodeId("By"), TestAgentPath, TestJobPath)
  private val C = JobNode(NodeId("C"), TestAgentPath, TestJobPath)

  private val TestWorkflowPath = WorkflowPath("/WORKFLOW")

  private val TestOrder = Order(OrderId("TEST"), NodeKey(TestWorkflowPath, A.id), Order.Ready, Some(Order.AttachedTo.Agent(TestAgentPath)),
    Payload(Map("VARIABLE" → "VALUE")))
  private val BxOrderId = OrderId("TEST/Bx")  // First NodeId "Bx" becomes OutletId
  private val ByOrderId = OrderId("TEST/By")
}
