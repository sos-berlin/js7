package js7.agent.data

import js7.base.problem.Checked._
import js7.data.agent.AgentId
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.{EventId, JournaledState}
import js7.data.order.Order.{Forked, Ready}
import js7.data.order.OrderEvent.{OrderAdded, OrderAttachedToAgent, OrderForked}
import js7.data.order.{Order, OrderId}
import js7.data.workflow.WorkflowEvent.WorkflowAttached
import js7.data.workflow.position._
import js7.data.workflow.{Workflow, WorkflowPath}
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AsyncFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class AgentStateTest extends AsyncFreeSpec
{
  private val workflowId = WorkflowPath("/WORKFLOW") ~ "1.0"
  private val agentState = AgentState(
    EventId(1000),
    JournaledState.Standards.empty,
    Map(OrderId("ORDER") -> Order.fromOrderAdded(OrderId("ORDER"), OrderAdded(workflowId))),
    Map(workflowId -> Workflow.of(workflowId)))

  "estimatedSnapshotSize" in {
    assert(agentState.estimatedSnapshotSize == 4)
    for (n <- agentState.toSnapshotObservable.countL.runToFuture)
      yield assert(n == agentState.estimatedSnapshotSize - 2)
  }

  "toSnapshotObservable" in {
    for (list <- agentState.toSnapshotObservable.toListL.runToFuture)
      yield assert(list ==
        agentState.idToWorkflow.values ++
          agentState.idToOrder.values.toList)
  }

  "applyEvent" in {
    val orderId = OrderId("ORDER")
    val childOrderId = OrderId("ORDER") | "BRANCH"
    val workflowId = WorkflowPath("/WORKFLOW") ~ "1.0"
    val workflow = Workflow.of(workflowId)
    val agentId = AgentId("AGENT")
    var agentState = AgentState.empty

    agentState = agentState.applyEvent(NoKey <-: WorkflowAttached(workflow)).orThrow
    agentState = agentState.applyEvent(orderId <-: OrderAttachedToAgent(workflowId, Order.Ready, Map.empty, Nil, agentId, None, None, false, false)).orThrow
    agentState = agentState.applyEvent(orderId <-: OrderForked(Seq(OrderForked.Child("BRANCH", childOrderId)))).orThrow
    assert(agentState == AgentState(
      EventId.BeforeFirst,
      JournaledState.Standards.empty,
      Map(
        orderId -> Order(orderId, workflowId, Forked(Seq(Forked.Child("BRANCH", childOrderId))), attachedState = Some(Order.Attached(agentId))),
        childOrderId -> Order(childOrderId, workflowId /: (Position(0) / "fork+BRANCH" % 0), Ready, attachedState = Some(Order.Attached(agentId)), parent = Some(orderId))),
      Map(workflowId -> workflow)
    ))
  }
}
