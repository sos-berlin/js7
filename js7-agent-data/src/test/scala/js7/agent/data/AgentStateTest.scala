package js7.agent.data

import com.softwaremill.diffx
import com.softwaremill.diffx.generic.auto._
import io.circe.syntax.EncoderOps
import java.nio.file.Paths
import js7.agent.data.ordersource.{FileOrderSourceState, FileOrderSourcesState}
import js7.base.auth.UserId
import js7.base.circeutils.CirceUtils.{JsonStringInterpolator, RichCirceEither}
import js7.base.io.file.watch.DirectoryState
import js7.base.problem.Checked._
import js7.data.agent.AgentId
import js7.data.cluster.ClusterState
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.{EventId, JournalState, JournaledState}
import js7.data.order.Order.{Forked, Ready}
import js7.data.order.OrderEvent.{OrderAdded, OrderAttachedToAgent, OrderForked}
import js7.data.order.{Order, OrderId}
import js7.data.ordersource.{FileOrderSource, OrderSourceId}
import js7.data.workflow.WorkflowEvent.WorkflowAttached
import js7.data.workflow.position._
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tester.CirceJsonTester.removeJNull
import monix.execution.Scheduler.Implicits.global
import monix.reactive.Observable
import org.scalatest.freespec.AsyncFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class AgentStateTest extends AsyncFreeSpec
{
  private val workflowId = WorkflowPath("WORKFLOW") ~ "1.0"
  private val agentState = AgentState(
    EventId(1000),
    JournaledState.Standards(
      JournalState(Map(UserId("USER") -> 500L)),
      ClusterState.Empty),
    Map(OrderId("ORDER") -> Order.fromOrderAdded(OrderId("ORDER"), OrderAdded(workflowId))),
    Map(workflowId -> Workflow.of(workflowId)),
    FileOrderSourcesState.fromIterable(Seq(
      FileOrderSourceState(
        FileOrderSource(
          OrderSourceId("ORDER-SOURCE-ID"),
          WorkflowPath("WORKFLOW"),
          AgentId("AGENT"),
        "/DIRECTORY",
        Some("""\.csv""".r.pattern)),
        DirectoryState.fromIterable(Seq(
          DirectoryState.Entry(Paths.get("/DIRECTORY/1.csv")),
          DirectoryState.Entry(Paths.get("/DIRECTORY/2.csv"))))))))

  "estimatedSnapshotSize" in {
    assert(agentState.estimatedSnapshotSize == 6)
    for (n <- agentState.toSnapshotObservable.countL.runToFuture)
      yield assert(n == agentState.estimatedSnapshotSize)
  }

  "Snapshot JSON" in {
    import AgentState.snapshotObjectJsonCodec
    agentState.toSnapshotObservable.map(_.asJson).map(removeJNull).toListL.runToFuture
      .flatMap { jsons =>
        assert(jsons == List(
          json"""{
            "TYPE": "JournalState",
            "userIdToReleasedEventId": {
              "USER": 500
            }
          }""",
          json"""{
            "TYPE": "Workflow",
            "path": "WORKFLOW",
            "versionId": "1.0",
            "instructions": []
          }""",
          json"""{
            "TYPE": "Order",
            "id": "ORDER",
            "workflowPosition": {
              "workflowId": {
                "path": "WORKFLOW",
                "versionId": "1.0"
              },
              "position": [ 0 ]
             },
            "state": {
              "TYPE": "Fresh"
            }
          }""",
          json"""{
            "TYPE": "FileOrderSource.Header",
            "orderSource": {
              "id": "ORDER-SOURCE-ID",
              "workflowPath": "WORKFLOW",
              "agentId": "AGENT",
              "directory": "/DIRECTORY",
              "pattern": "\\.csv",
              "itemRevision": 0
            }
          }""",
          json"""{
            "TYPE": "FileOrderSource.File",
            "orderSourceId": "ORDER-SOURCE-ID",
            "path": "/DIRECTORY/1.csv"
          }""",
          json"""{
            "TYPE": "FileOrderSource.File",
            "orderSourceId": "ORDER-SOURCE-ID",
            "path": "/DIRECTORY/2.csv"
          }"""))

        AgentState
          .fromObservable(
            Observable.fromIterable(jsons)
              .map(o => AgentState.snapshotObjectJsonCodec.decodeJson(o).toChecked.orThrow))
          .runToFuture
          .flatMap { fromSnapshot =>
            val a = agentState.copy(eventId = 0)
            if (fromSnapshot != a) {  // Diff.compare do not uses our equals implementation
              val diffResult = diffx.Diff.compare(fromSnapshot, a)
              fail(diffResult.show)
            } else
              succeed
          }
    }
  }

  "applyEvent" in {
    val orderId = OrderId("ORDER")
    val childOrderId = OrderId("ORDER") | "BRANCH"
    val workflowId = WorkflowPath("WORKFLOW") ~ "1.0"
    val workflow = Workflow.of(workflowId)
    val agentId = AgentId("AGENT")
    var agentState = AgentState.empty

    agentState = agentState.applyEvent(NoKey <-: WorkflowAttached(workflow)).orThrow
    agentState = agentState.applyEvent(orderId <-:
      OrderAttachedToAgent(
        workflowId, Order.Ready, Map.empty, None, Nil, agentId, None, None, false, false))
      .orThrow
    agentState = agentState.applyEvent(orderId <-: OrderForked(Seq(OrderForked.Child("BRANCH", childOrderId))))
      .orThrow
    assert(agentState == AgentState(
      EventId.BeforeFirst,
      JournaledState.Standards.empty,
      Map(
        orderId ->
          Order(orderId, workflowId, Forked(Seq(Forked.Child("BRANCH", childOrderId))),
            attachedState = Some(Order.Attached(agentId))),
        childOrderId ->
          Order(childOrderId, workflowId /: (Position(0) / "fork+BRANCH" % 0), Ready,
            attachedState = Some(Order.Attached(agentId)), parent = Some(orderId))),
      Map(workflowId -> workflow),
      FileOrderSourcesState.empty))
  }
}
