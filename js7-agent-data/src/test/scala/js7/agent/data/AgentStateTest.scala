package js7.agent.data

import com.softwaremill.diffx
import com.softwaremill.diffx.generic.auto._
import io.circe.syntax.EncoderOps
import java.nio.file.Paths
import js7.agent.data.orderwatch.{AllFileWatchesState, FileWatchState}
import js7.base.auth.UserId
import js7.base.circeutils.CirceUtils.{JsonStringInterpolator, RichCirceEither}
import js7.base.io.file.watch.DirectoryState
import js7.base.problem.Checked._
import js7.base.time.ScalaTime._
import js7.base.utils.SimplePattern
import js7.data.agent.AgentPath
import js7.data.cluster.ClusterState
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.{EventId, JournalState, JournaledState}
import js7.data.item.BasicItemEvent.ItemAttachedToAgent
import js7.data.item.ItemRevision
import js7.data.job.{JobResource, JobResourcePath}
import js7.data.order.Order.{Forked, Ready}
import js7.data.order.OrderEvent.{OrderAdded, OrderAttachedToAgent, OrderForked}
import js7.data.order.{Order, OrderId}
import js7.data.orderwatch.{FileWatch, OrderWatchPath}
import js7.data.value.expression.Expression
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
  private val jobResourcePath = JobResourcePath("JOB-RESOURCE")
  private val agentState = AgentState(
    EventId(1000),
    JournaledState.Standards(
      JournalState(Map(UserId("USER") -> 500L)),
      ClusterState.Empty),
    Map(
      OrderId("ORDER") -> Order.fromOrderAdded(OrderId("ORDER"), OrderAdded(workflowId))),
    Map(
      workflowId -> Workflow.of(workflowId)),
    AllFileWatchesState.fromIterable(Seq(
      FileWatchState(
        FileWatch(
          OrderWatchPath("ORDER-SOURCE-ID"),
          WorkflowPath("WORKFLOW"),
          AgentPath("AGENT"),
          "/DIRECTORY",
          Some(SimplePattern("""\.csv""".r.pattern.pattern)),
          Some(Expression.NamedValue("0")),
          3.s,
          Some(ItemRevision(7))),
        DirectoryState.fromIterable(Seq(
          DirectoryState.Entry(Paths.get("/DIRECTORY/1.csv")),
          DirectoryState.Entry(Paths.get("/DIRECTORY/2.csv"))))))),
    Map(
      jobResourcePath -> JobResource(jobResourcePath)))

  "estimatedSnapshotSize" in {
    assert(agentState.estimatedSnapshotSize == 7)
    for (n <- agentState.toSnapshotObservable.countL.runToFuture)
      yield assert(n == agentState.estimatedSnapshotSize)
  }

  "Snapshot JSON" in {
    import AgentState.implicits.snapshotObjectJsonCodec
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
            "TYPE": "FileWatchState",
            "fileWatch": {
              "id": "ORDER-SOURCE-ID",
              "workflowPath": "WORKFLOW",
              "agentId": "AGENT",
              "directory": "/DIRECTORY",
              "pattern": "\\.csv",
              "delay": 3,
              "orderIdExpression": "$$0",
              "itemRevision": 7
            }
          }""",
          json"""{
            "TYPE": "FileWatchState.File",
            "orderWatchPath": "ORDER-SOURCE-ID",
            "path": "/DIRECTORY/1.csv"
          }""",
          json"""{
            "TYPE": "FileWatchState.File",
            "orderWatchPath": "ORDER-SOURCE-ID",
            "path": "/DIRECTORY/2.csv"
          }""",
          json"""{
            "TYPE": "JobResource",
            "id": "JOB-RESOURCE",
            "env": {}
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
    val agentId = AgentPath("AGENT")
    var agentState = AgentState.empty

    agentState = agentState.applyEvent(NoKey <-: ItemAttachedToAgent(workflow)).orThrow
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
      AllFileWatchesState.empty,
      Map.empty))
  }
}
