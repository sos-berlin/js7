package js7.agent.data

import js7.data.event.{EventId, JournaledState}
import js7.data.order.OrderEvent.OrderAdded
import js7.data.order.{Order, OrderId}
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
}
