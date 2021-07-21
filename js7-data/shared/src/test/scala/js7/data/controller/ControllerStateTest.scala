package js7.data.controller

import com.softwaremill.diffx.generic.auto._
import js7.base.auth.UserId
import js7.base.circeutils.CirceUtils._
import js7.base.crypt.silly.SillySigner
import js7.base.problem.Checked._
import js7.base.time.ScalaTime._
import js7.base.time.Timestamp
import js7.base.utils.Collections.RichMap
import js7.base.utils.Collections.implicits._
import js7.base.web.Uri
import js7.data.agent.{AgentPath, AgentRef, AgentRefState}
import js7.data.board.{Board, BoardPath, BoardState, Notice, NoticeExpectation, NoticeId}
import js7.data.cluster.{ClusterSetting, ClusterState, ClusterStateSnapshot, ClusterTiming}
import js7.data.controller.ControllerStateTest._
import js7.data.event.SnapshotMeta.SnapshotEventId
import js7.data.event.{EventId, JournalState, JournaledState}
import js7.data.item.BasicItemEvent.{ItemAttachable, ItemDeletionMarked}
import js7.data.item.ItemAttachedState.{Attachable, Attached}
import js7.data.item.SignedItemEvent.SignedItemAdded
import js7.data.item.UnsignedSimpleItemEvent.UnsignedSimpleItemAdded
import js7.data.item.VersionedEvent.{VersionAdded, VersionedItemAdded, VersionedItemChanged}
import js7.data.item.{ItemRevision, ItemSigner, Repo, VersionId}
import js7.data.job.{JobResource, JobResourcePath, ShellScriptExecutable}
import js7.data.lock.{Lock, LockPath, LockState}
import js7.data.node.NodeId
import js7.data.order.{Order, OrderId}
import js7.data.orderwatch.OrderWatchState.{HasOrder, VanishedAck}
import js7.data.orderwatch.{AllOrderWatchesState, ExternalOrderKey, ExternalOrderName, FileWatch, OrderWatchPath, OrderWatchState}
import js7.data.value.expression.ExpressionParser.expr
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.instructions.{Execute, ExpectNotice, LockInstruction}
import js7.data.workflow.position.Position
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tester.CirceJsonTester.testJson
import js7.tester.DiffxAssertions.assertEqual
import monix.execution.Scheduler.Implicits.global
import monix.reactive.Observable
import org.scalatest.freespec.AsyncFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class ControllerStateTest extends AsyncFreeSpec
{
  "estimatedSnapshotSize" in {
    assert(controllerState.estimatedSnapshotSize == 17)
    for (n <- controllerState.toSnapshotObservable.countL.runToFuture) yield
      assert(controllerState.estimatedSnapshotSize == n)
  }

  "pathToSimpleItem" in {
    val sum = (controllerState.pathToAgentRefState ++
      controllerState.pathToLockState ++
      controllerState.allOrderWatchesState.pathToOrderWatchState).map(_._2.item) ++
      controllerState.pathToSignedSimpleItem.values.map(_.value)
    assert(controllerState.pathToSimpleItem.toMap == sum.toKeyedMap(_.key))
  }

  "toSnapshotObservable" in {
    for (list <- controllerState.toSnapshotObservable.toListL.runToFuture)
      yield assert(list ==
        Seq(
          SnapshotEventId(1001L),
          JournalState(Map(UserId("A") -> EventId(1000))),
          ClusterStateSnapshot(
            ClusterState.Coupled(
              ClusterSetting(
                Map(
                  NodeId("A") -> Uri("https://A"),
                  NodeId("B") -> Uri("https://B")),
                activeId = NodeId("A"),
                Seq(ClusterSetting.Watch(Uri("https://CLUSTER-WATCH"))),
                ClusterTiming(10.s, 20.s)))),
          controllerState.controllerMetaState
        ) ++
          controllerState.pathToAgentRefState.values ++
          controllerState.pathToLockState.values ++
          Seq(board) ++
          boardState.notices.map(Notice.Snapshot(board.path, _)) ++
          Seq(
            UnsignedSimpleItemAdded(FileWatch(
              fileWatch.path,
              workflow.path,
              agentRef.path,
              "/tmp/directory",
              itemRevision = Some(ItemRevision(7)))),
            OrderWatchState.ExternalOrderSnapshot(
              fileWatch.path,
              ExternalOrderName("ORDER-NAME"),
              HasOrder(OrderId("ORDER"), Some(VanishedAck))),
            SignedItemAdded(signedJobResource),
            VersionAdded(versionId),
            VersionedItemAdded(signedWorkflow),
            ItemAttachable(jobResource.path, agentRef.path),
            ItemDeletionMarked(fileWatch.path)
          ) ++
          controllerState.idToOrder.values)
  }

  "isWorkflowUsedByOrders" in {
    assert(controllerState.isWorkflowUsedByOrders == Set(workflow.id))
  }

  "isWorkflowUsedByOrders does not include orderless workflows" in {
    val v = VersionId("X")
    val workflowId = WorkflowPath("X") ~ v
    val controllerStateWithOrderlessWorkflow = controllerState
      .applyEvents(Seq(
        VersionAdded(v),
        VersionedItemAdded(itemSigner.sign(Workflow(workflowId, Nil)))))
      .orThrow
    assert(controllerStateWithOrderlessWorkflow.isWorkflowUsedByOrders == Set(workflow.id))
  }

  "pathToReferencingItemKeys" in {
    assert(controllerState.pathToReferencingItemKeys.mapValuesStrict(_.toSet) == Map(
      lock.path -> Set(workflow.id),
      board.path -> Set(workflow.id),
      agentRef.path -> Set(fileWatch.path, workflow.id),
      jobResource.path -> Set(workflow.id),
      workflow.path -> Set(fileWatch.path)))
  }

  "pathToReferencingItemKeys does not return hidden workflows without orders" in {
    val v = VersionId("x")
    val changedWorkflowId = workflow.path ~ v
    val controllerState = ControllerStateTest.controllerState
      .applyEvents(Seq(
        VersionAdded(v),
        VersionedItemChanged(itemSigner.sign(workflow.copy(id = changedWorkflowId)))))
      .orThrow
      .copy(idToOrder = Map.empty)
    // The original workflow is still in use by an order and not deleted
    assert(controllerState.pathToReferencingItemKeys.view.mapValues(_.toSet).toMap
      == Map(
        lock.path -> Set(workflow.id, changedWorkflowId),
        board.path -> Set(workflow.id, changedWorkflowId),
        agentRef.path -> Set(workflow.id, changedWorkflowId, fileWatch.path),
        jobResource.path -> Set(workflow.id, changedWorkflowId),
        workflow.path -> Set(fileWatch.path)))
  }

  "fromIterator is the reverse of toSnapshotObservable" in {
    val task = for {
      elems <- controllerState.toSnapshotObservable.toListL
      expectedState <- ControllerState.fromObservable(Observable.fromIterable(elems))
    } yield assertEqual(controllerState, expectedState)
    task.runToFuture
  }

  private val expectedSnapshotJsonArray =
    json"""[
      {
        "TYPE": "SnapshotEventId",
        "eventId": 1001
      }, {
        "TYPE": "JournalState",
        "userIdToReleasedEventId": {
          "A": 1000
        }
      }, {
        "TYPE": "ClusterStateSnapshot",
        "clusterState": {
          "TYPE": "Coupled",
          "setting": {
            "idToUri": {
              "A": "https://A",
              "B": "https://B"
            },
            "activeId": "A",
            "clusterWatches": [ { "uri": "https://CLUSTER-WATCH" } ],
            "timing": {
              "heartbeat": 10,
              "heartbeatTimeout": 20
            }
          }
        }
      }, {
        "TYPE": "ControllerMetaState",
        "controllerId": "CONTROLLER-ID",
        "startedAt": 1558699200000,
        "timezone": "Europe/Berlin"
      }, {
        "TYPE": "AgentRefState",
        "agentRef": {
          "path": "AGENT",
          "uri": "https://AGENT",
          "itemRevision": 0
        },
        "couplingState": {
          "TYPE": "Reset"
        },
        "eventId": 7
      }, {
        "TYPE": "LockState",
        "lock": {
          "path": "LOCK",
          "limit": 1,
          "itemRevision": 7
        },
        "acquired": {
          "TYPE": "Available"
        },
        "queue": []
      }, {
        "TYPE": "Board",
        "path": "BOARD",
        "toNotice": "$$orderId",
        "expectingOrderToNoticeId": "$$orderId",
        "endOfLife": "$$epochMillis + 24 * 3600 * 1000",
        "itemRevision": 7
      }, {
        "TYPE": "Notice",
        "boardPath": "BOARD",
        "id": "NOTICE-1",
        "endOfLife": 10086400000
      }, {
        "TYPE": "UnsignedSimpleItemAdded",
        "item": {
          "TYPE": "FileWatch",
          "path": "WATCH",
          "workflowPath": "WORKFLOW",
          "agentPath": "AGENT",
          "directory": "/tmp/directory",
          "delay": 0,
          "itemRevision": 7
        }
      }, {
        "TYPE": "ExternalOrder",
        "orderWatchPath": "WATCH",
        "externalOrderName": "ORDER-NAME",
        "state": {
          "TYPE": "HasOrder",
          "orderId": "ORDER",
          "queued": {
            "TYPE": "VanishedAck"
          }
        }
      }, {
        "TYPE": "SignedItemAdded",
        "signed": {
          "string": "{\"TYPE\":\"JobResource\",\"path\":\"JOB-RESOURCE\",\"variables\":{},\"env\":{}}",
          "signature": {
            "TYPE": "Silly",
            "signatureString": "SILLY-SIGNATURE"
          }
        }
      }, {
        "TYPE": "VersionAdded",
        "versionId": "1.0"
      }, {
        "TYPE": "VersionedItemAdded",
        "signed": {
          "signature": {
            "TYPE": "Silly",
            "signatureString": "SILLY-SIGNATURE"
          },
          "string": "{\"TYPE\":\"Workflow\",\"path\":\"WORKFLOW\",\"versionId\":\"1.0\",\"instructions\":[{\"TYPE\":\"Lock\",\"lockPath\":\"LOCK\",\"lockedWorkflow\":{\"instructions\":[{\"TYPE\":\"Execute.Anonymous\",\"job\":{\"agentPath\":\"AGENT\",\"executable\":{\"TYPE\":\"ShellScriptExecutable\",\"script\":\"\"},\"jobResourcePaths\":[\"JOB-RESOURCE\"],\"parallelism\":1}}]}},{\"TYPE\":\"ExpectNotice\",\"boardPath\":\"BOARD\"}]}"
        }
      }, {
        "TYPE": "ItemAttachable",
        "agentPath": "AGENT",
        "key": "JobResource:JOB-RESOURCE"
      }, {
        "TYPE": "ItemDeletionMarked",
        "key": "OrderWatch:WATCH"
      }, {
        "TYPE": "Order",
        "id": "ORDER",
        "state": {
          "TYPE": "Fresh"
        },
        "externalOrderKey": {
          "orderWatchPath": "WATCH",
          "name": "ORDER-NAME"
        },
        "workflowPosition": {
          "position": [ 0 ],
          "workflowId": {
            "path": "WORKFLOW",
            "versionId": "1.0"
          }
        }
      }, {
        "TYPE": "Order",
        "id": "ORDER-EXPECTING-NOTICE",
        "state": {
          "TYPE": "ExpectingNotice",
          "noticeId": "NOTICE-2"
        },
        "workflowPosition": {
          "position": [ 1 ],
          "workflowId": {
            "path": "WORKFLOW",
            "versionId": "1.0"
          }
        }
      }
    ]"""

  "toSnapshotObservable JSON" in {
    implicit val x = ControllerState.snapshotObjectJsonCodec
    for {
      jsonArray <- controllerState.toSnapshotObservable.toListL.runToFuture
      assertion <- testJson(jsonArray, expectedSnapshotJsonArray)
    } yield assertion
  }

  "ControllerStateBuilder.addSnapshotObject" in {
    ControllerState.snapshotObjectJsonCodec
    val builder = new ControllerStateBuilder
    expectedSnapshotJsonArray.asArray.get
      .map(json => ControllerState.snapshotObjectJsonCodec.decodeJson(json).toChecked.orThrow)
      .foreach(builder.addSnapshotObject)
    builder.onAllSnapshotsAdded()
    assert(builder.result() == controllerState)
  }

  "keyToItem" in {
    assert(!controllerState.keyToItem.contains(WorkflowPath("UNKNOWN") ~ "1"))

    assert(controllerState.keyToItem.get(jobResource.path) == Some(jobResource))
    assert(controllerState.keyToItem.get(agentRef.path) == Some(agentRef))
    assert(controllerState.keyToItem.get(lock.path) == Some(lock))
    assert(controllerState.keyToItem.get(fileWatch.path) == Some(fileWatch))
    assert(controllerState.keyToItem.get(workflow.id) == Some(workflow))

    assert(controllerState.keyToItem.keySet == Set(
      jobResource.path, agentRef.path, lock.path, fileWatch.path, workflow.id))
  }
}

object ControllerStateTest
{
  private val jobResource = JobResource(JobResourcePath("JOB-RESOURCE"))
  private lazy val itemSigner = new ItemSigner(SillySigner.Default, ControllerState.signableItemJsonCodec)
  private lazy val signedJobResource = itemSigner.sign(jobResource)
  private val orderId = OrderId("ORDER")
  private val expectingNoticeOrderId = OrderId("ORDER-EXPECTING-NOTICE")
  private val agentRef = AgentRef(AgentPath("AGENT"), Uri("https://AGENT"), Some(ItemRevision(0)))
  private val lock = Lock(LockPath("LOCK"), itemRevision = Some(ItemRevision(7)))
  private val notice = Notice(NoticeId("NOTICE-1"), Timestamp.ofEpochMilli(10_000_000_000L + 24*3600*1000))
  private val noticeExpectation = NoticeExpectation(NoticeId("NOTICE-2"), Seq(expectingNoticeOrderId))
  private val board = Board(
    BoardPath("BOARD"),
    toNotice = expr("$orderId"),
    expectingOrderToNoticeId = expr("$orderId"),
    endOfLife = expr("$epochMillis + 24*3600*1000"),
    itemRevision = Some(ItemRevision(7)))
  private val boardState = BoardState(
    board,
    Map(
      notice.id -> notice,
      noticeExpectation.id -> noticeExpectation))
  private val versionId = VersionId("1.0")
  private[controller] val workflow = Workflow(WorkflowPath("WORKFLOW") ~ versionId, Seq(
    LockInstruction(lock.path, None, Workflow.of(
      Execute(WorkflowJob(agentRef.path, ShellScriptExecutable(""), jobResourcePaths = Seq(jobResource.path))))),
    ExpectNotice(board.path)))
  private val signedWorkflow = itemSigner.sign(workflow)
  private[controller] val fileWatch = FileWatch(
    OrderWatchPath("WATCH"),
    workflow.path,
    AgentPath("AGENT"),
    "/tmp/directory",
    itemRevision = Some(ItemRevision(7)))

  // Also used by ControllerStateExecutorTest
  private[controller] lazy val controllerState = ControllerState(
    EventId(1001),
    JournaledState.Standards(
      JournalState(Map(UserId("A") -> EventId(1000))),
      ClusterState.Coupled(
        ClusterSetting(
          Map(
            NodeId("A") -> Uri("https://A"),
            NodeId("B") -> Uri("https://B")),
          activeId = NodeId("A"),
          Seq(ClusterSetting.Watch(Uri("https://CLUSTER-WATCH"))),
          ClusterTiming(10.s, 20.s)))),
    ControllerMetaState(ControllerId("CONTROLLER-ID"), Timestamp("2019-05-24T12:00:00Z"), timezone = "Europe/Berlin"),
    Map(
      agentRef.path -> AgentRefState(
        agentRef, None, None, AgentRefState.Reset, EventId(7))),
    Map(
      lock.path -> LockState(lock)),
    Map(
      boardState.path -> boardState),
    AllOrderWatchesState(Map(
      fileWatch.path -> OrderWatchState(
        fileWatch,
        Map(agentRef.path -> Attached(Some(ItemRevision(7)))),
        Map(
          ExternalOrderName("ORDER-NAME") -> HasOrder(OrderId("ORDER"), Some(VanishedAck)))))),
    Repo.empty.applyEvents(Seq(
      VersionAdded(versionId),
      VersionedItemAdded(signedWorkflow))).orThrow,
    Map(
      jobResource.path -> signedJobResource),
    Map(
      jobResource.path -> Map(agentRef.path -> Attachable)),
    deletionMarkedItems = Set(fileWatch.path),
    Seq(
      Order(orderId, workflow.id /: Position(0), Order.Fresh,
        externalOrderKey = Some(ExternalOrderKey(fileWatch.path, ExternalOrderName("ORDER-NAME")))),
      Order(expectingNoticeOrderId, workflow.id /: Position(1), Order.ExpectingNotice(noticeExpectation.id))
    ).toKeyedMap(_.id))
}
