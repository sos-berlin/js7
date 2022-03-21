package js7.agent.data

import com.softwaremill.diffx
import com.softwaremill.diffx.generic.auto._
import io.circe.syntax.EncoderOps
import java.io.File.separator
import java.nio.file.Paths
import java.util.UUID
import js7.agent.data.AgentState.AgentMetaState
import js7.agent.data.event.AgentEvent.AgentDedicated
import js7.agent.data.orderwatch.{AllFileWatchesState, FileWatchState}
import js7.base.auth.UserId
import js7.base.circeutils.CirceUtils.{JsonStringInterpolator, RichCirceEither}
import js7.base.crypt.silly.SillySigner
import js7.base.io.file.watch.DirectoryState
import js7.base.problem.Checked._
import js7.base.problem.Problem
import js7.base.time.ScalaTime._
import js7.base.time.Timezone
import js7.base.utils.SimplePattern
import js7.base.web.Uri
import js7.data.agent.{AgentPath, AgentRunId}
import js7.data.calendar.{Calendar, CalendarPath}
import js7.data.cluster.ClusterState
import js7.data.controller.ControllerId
import js7.data.event.JournalEvent.SnapshotTaken
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.{EventId, JournalId, JournalState, SnapshotableState}
import js7.data.item.BasicItemEvent.{ItemAttachedToMe, SignedItemAttachedToMe}
import js7.data.item.{ItemRevision, ItemSigner}
import js7.data.job.{JobResource, JobResourcePath}
import js7.data.order.Order.{Forked, Ready}
import js7.data.order.OrderEvent.{OrderAdded, OrderAttachedToAgent, OrderForked}
import js7.data.order.{Order, OrderId}
import js7.data.orderwatch.{FileWatch, OrderWatchPath}
import js7.data.subagent.{SubagentId, SubagentItem, SubagentItemState, SubagentSelection, SubagentSelectionId}
import js7.data.value.expression.Expression
import js7.data.value.expression.ExpressionParser.expr
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
  private val subagentItem = SubagentItem(
    SubagentId("SUBAGENT"),
    AgentPath("AGENT"),
    Uri("https://localhost:0"),
    disabled = false,
    Some(ItemRevision(7)))

  private val subagentSelection = SubagentSelection(
    SubagentSelectionId("SELECTION"),
    Map(subagentItem.id -> 1),
    Some(ItemRevision(7)))

  private val workflow = Workflow(WorkflowPath("WORKFLOW") ~ "1.0", Nil)
  private val jobResource = JobResource(JobResourcePath("JOB-RESOURCE"))
  private val itemSigner = new ItemSigner(SillySigner.Default, AgentState.signableItemJsonCodec)
  private val signedWorkflow = itemSigner.sign(Workflow(WorkflowPath("SIGNED-WORKFLOW") ~ "1.0", Nil))
  private val signedJobResource = itemSigner.sign(JobResource(JobResourcePath("SIGNED-JOBRESOURCE")))

  private val calendar = Calendar(
    CalendarPath("CALENDAR"),
    Timezone("Europe/Mariehamn"),
    dateOffset = 6.h,
    orderIdToDatePattern = "#([^#]+)#.*",
    periodDatePattern = "yyyy-MM-dd",
    Some(ItemRevision(1)))

  private val fileWatch = FileWatch(
    OrderWatchPath("ORDER-SOURCE-ID"),
    WorkflowPath("WORKFLOW"),
    AgentPath("AGENT"),
    expr(s"'${separator}DIRECTORY'"),
    Some(SimplePattern("""\.csv""".r.pattern.pattern)),
    Some(Expression.NamedValue("0")),
    3.s,
    Some(ItemRevision(7)))

  private val agentState = AgentState(
    EventId(1000),
    SnapshotableState.Standards(
      JournalState(Map(UserId("USER") -> 500L)),
      ClusterState.Empty),
    AgentMetaState(
      Some(SubagentId("SUBAGENT")),
      AgentPath("AGENT"),
      AgentRunId(JournalId(UUID.fromString("00112233-4455-6677-8899-AABBCCDDEEFF"))),
      ControllerId("CONTROLLER")),
    Map(
      subagentItem.id -> SubagentItemState.initial(subagentItem)),
    Map(
      subagentSelection.id -> subagentSelection),
    Map(
      OrderId("ORDER") -> Order.fromOrderAdded(OrderId("ORDER"), OrderAdded(workflow.id))),
    Map(
      workflow.id -> workflow,
      signedWorkflow.value.id -> signedWorkflow.value),
    AllFileWatchesState.fromIterable(Seq(
      FileWatchState(
        fileWatch,
        DirectoryState.fromIterable(Seq(
          DirectoryState.Entry(Paths.get("/DIRECTORY/1.csv")),
          DirectoryState.Entry(Paths.get("/DIRECTORY/2.csv"))))))),
    Map(
      jobResource.path -> jobResource,
      signedJobResource.value.path -> signedJobResource.value),
    Map(
      calendar.path -> calendar),
    Map(
      signedJobResource.value.path -> signedJobResource,
      signedWorkflow.value.id -> signedWorkflow))

  "isDedicated, isFreshlyDedicated" - {
    "empty" in {
      assert(!AgentState.empty.isDedicated)
    }

    "AgentState example" in {
      assert(agentState.isDedicated)
      assert(!agentState.isFreshlyDedicated)
    }

    "After snapshot" in {
      val afterSnapshot = AgentState.empty.applyEvent(SnapshotTaken).orThrow
      assert(!afterSnapshot.isDedicated)
      assert(!afterSnapshot.isFreshlyDedicated)
    }

    val dedicated = AgentDedicated(
      Some(SubagentId("SUBAGENT")),
      AgentPath("A"),
      AgentRunId(JournalId.random()),
      ControllerId("C"))
    "AgentDedicated" in {
      val dedicatedState = AgentState.empty.applyEvent(dedicated).orThrow
      assert(dedicatedState.isDedicated)
      assert(dedicatedState.isFreshlyDedicated)
    }
  }

  "estimatedSnapshotSize" in {
    assert(agentState.estimatedSnapshotSize == 13)
    for (n <- agentState.toSnapshotObservable.countL.runToFuture)
      yield assert(n == agentState.estimatedSnapshotSize)
  }

  "Snapshot JSON" in {
    implicit val x = AgentState.snapshotObjectJsonCodec
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
            "TYPE": "AgentMetaState",
            "subagentId": "SUBAGENT",
            "agentPath": "AGENT",
            "agentRunId": "ABEiM0RVZneImaq7zN3u_w",
            "controllerId": "CONTROLLER"
          }""",
          json"""{
            "TYPE": "SubagentItemState",
            "subagentItem": {
              "id": "SUBAGENT",
              "agentPath": "AGENT",
              "uri": "https://localhost:0",
              "disabled": false,
              "itemRevision": 7
             },
             "couplingState": {
               "TYPE" : "Reset"
             },
             "eventId": 0
          }""",
          json"""{
            "TYPE": "SubagentSelection",
            "id": "SELECTION",
            "subagentToPriority": {
              "SUBAGENT": 1
            },
            "itemRevision": 7
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
              "path": "ORDER-SOURCE-ID",
              "workflowPath": "WORKFLOW",
              "agentPath": "AGENT",
              "directoryExpr": "'${separator}DIRECTORY'",
              "pattern": "\\.csv",
              "delay": 3,
              "orderIdExpression": "$$0",
              "itemRevision": 7
            }
          }""",
          json"""{
            "TYPE": "FileWatchState.File",
            "orderWatchPath": "ORDER-SOURCE-ID",
            "path": "${separator}DIRECTORY${separator}1.csv"
          }""",
          json"""{
            "TYPE": "FileWatchState.File",
            "orderWatchPath": "ORDER-SOURCE-ID",
            "path": "${separator}DIRECTORY${separator}2.csv"
          }""",

          json"""{
            "TYPE" : "SignedItemAdded",
            "signed" : {
              "string" : "{\"TYPE\":\"JobResource\",\"path\":\"SIGNED-JOBRESOURCE\",\"variables\":{},\"env\":{}}",
              "signature" : {
                "TYPE" : "Silly",
                "signatureString" : "SILLY-SIGNATURE"
              }
            }
          }""",
          json"""{
            "TYPE" : "SignedItemAdded",
            "signed" : {
              "string" : "{\"TYPE\":\"Workflow\",\"path\":\"SIGNED-WORKFLOW\",\"versionId\":\"1.0\",\"instructions\":[]}",
              "signature" : {
                "TYPE" : "Silly",
                "signatureString" : "SILLY-SIGNATURE"
              }
            }
          }""",
          json"""{
            "TYPE": "Workflow",
            "path": "WORKFLOW",
            "versionId": "1.0",
            "instructions": []
          }""",
          json"""{
            "TYPE": "JobResource",
            "path": "JOB-RESOURCE",
            "variables": {},
            "env": {}
          }""",
          json"""{
            "TYPE": "Calendar",
            "path": "CALENDAR",
            "timezone": "Europe/Mariehamn",
            "dateOffset": 21600,
            "orderIdPattern": "#([^#]+)#.*",
            "periodDatePattern": "yyyy-MM-dd",
            "itemRevision": 1
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
              fail(diffResult.show())
            } else
              succeed
          }
    }
  }

  "Unknown TYPE for snapshotObjectJsonCodec" in {
    assert(AgentState.snapshotObjectJsonCodec
      .decodeJson(json"""{ "TYPE": "UNKNOWN" }""").toChecked == Left(Problem(
      """JSON DecodingFailure at : Unexpected JSON {"TYPE": "UNKNOWN", ...} for class 'AgentState.snapshotObjectJsonCodec'""")))
  }

  "Unknown TYPE for keyedEventJsonCodec" in {
    assert(AgentState.keyedEventJsonCodec
      .decodeJson(json"""{ "TYPE": "UNKNOWN" }""").toChecked == Left(Problem(
      """JSON DecodingFailure at : Unexpected JSON {"TYPE": "UNKNOWN", ...} for class 'AgentState.Event'""")))
  }

  "applyEvent" in {
    val orderId = OrderId("ORDER")
    val childOrderId = OrderId("ORDER") / "BRANCH"
    val workflowId = WorkflowPath("WORKFLOW") ~ "1.0"
    val workflow = Workflow.of(workflowId)
    val agentPath = AgentPath("AGENT")
    var agentState = AgentState.empty
    val meta = AgentMetaState(
      Some(subagentItem.id),
      AgentPath("AGENT"),
      AgentRunId(JournalId(UUID.fromString("11111111-2222-3333-4444-555555555555"))),
      ControllerId("CONTROLLER"))
    agentState = agentState.applyEvent(AgentDedicated(
      Some(subagentItem.id),
      meta.agentPath,
      meta.agentRunId,
      meta.controllerId)).orThrow
    agentState = agentState.applyEvent(NoKey <-: ItemAttachedToMe(workflow)).orThrow
    agentState = agentState.applyEvent(NoKey <-: ItemAttachedToMe(jobResource)).orThrow
    agentState = agentState.applyEvent(NoKey <-: SignedItemAttachedToMe(signedWorkflow)).orThrow
    agentState = agentState.applyEvent(NoKey <-: SignedItemAttachedToMe(signedJobResource)).orThrow
    agentState = agentState.applyEvent(orderId <-:
      OrderAttachedToAgent(
        workflowId, Order.Ready, Map.empty, None, None, Vector.empty, agentPath, None, None, false, false))
      .orThrow
    agentState = agentState.applyEvent(orderId <-: OrderForked(Vector(OrderForked.Child("BRANCH", childOrderId))))
      .orThrow
    assert(agentState == AgentState(
      EventId.BeforeFirst,
      SnapshotableState.Standards.empty,
      meta,
      Map.empty,
      Map.empty,
      Map(
        orderId ->
          Order(orderId, workflowId, Forked(Vector(Forked.Child("BRANCH", childOrderId))),
            attachedState = Some(Order.Attached(agentPath))),
        childOrderId ->
          Order(childOrderId, workflowId /: (Position(0) / "fork+BRANCH" % 0), Ready,
            attachedState = Some(Order.Attached(agentPath)), parent = Some(orderId))),
      Map(
        workflowId -> workflow,
        signedWorkflow.value.id -> signedWorkflow.value),
      AllFileWatchesState.empty,
      Map(
        jobResource.path -> jobResource,
        signedJobResource.value.path -> signedJobResource.value),
      Map.empty,
      Map(
        signedJobResource.value.path -> signedJobResource,
        signedWorkflow.value.id -> signedWorkflow)))
  }

  "keyToItem" in {
    assert(!agentState.keyToItem.contains(WorkflowPath("UNKNOWN") ~ "1"))
    assert(agentState.keyToItem.get(workflow.id) == Some(workflow))
    assert(agentState.keyToItem.get(jobResource.path) == Some(jobResource))
    assert(agentState.keyToItem.keySet ==
      Set(workflow.id, signedWorkflow.value.id,
        jobResource.path, signedJobResource.value.path,
      calendar.path, fileWatch.path, subagentItem.id))
  }
}
