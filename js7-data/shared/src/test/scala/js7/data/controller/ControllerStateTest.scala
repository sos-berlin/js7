package js7.data.controller

import cats.effect.unsafe.IORuntime
import cats.syntax.option.*
import js7.base.auth.UserId
import js7.base.circeutils.CirceUtils.*
import js7.base.crypt.silly.SillySigner
import js7.base.problem.Checked.*
import js7.base.test.OurAsyncTestSuite
import js7.base.time.ScalaTime.*
import js7.base.time.{Timestamp, Timezone}
import js7.base.utils.Collections.RichMap
import js7.base.utils.Collections.implicits.*
import js7.base.web.Uri
import js7.data.agent.{AgentPath, AgentRef, AgentRefState}
import js7.data.board.{BoardPath, BoardPathExpression, BoardState, GlobalBoard, GlobalNoticeKey, Notice, NoticeKey, NoticePlace}
import js7.data.calendar.{Calendar, CalendarPath, CalendarState}
import js7.data.cluster.{ClusterSetting, ClusterState, ClusterStateSnapshot, ClusterTiming, ClusterWatchId}
import js7.data.controller.ControllerStateTest.*
import js7.data.delegate.DelegateCouplingState
import js7.data.event.SnapshotMeta.SnapshotEventId
import js7.data.event.{EventId, JournalState, SnapshotableState, Stamped}
import js7.data.item.BasicItemEvent.{ItemAttachable, ItemDeletionMarked}
import js7.data.item.ItemAttachedState.Attachable
import js7.data.item.SignedItemEvent.SignedItemAdded
import js7.data.item.UnsignedSimpleItemEvent.{UnsignedSimpleItemAdded, UnsignedSimpleItemChanged}
import js7.data.item.VersionedEvent.{VersionAdded, VersionedItemAdded, VersionedItemChanged}
import js7.data.item.{ClientAttachments, InventoryItemKey, InventoryItemPath, ItemRevision, ItemSigner, Repo, UnsignedSimpleItemEvent, VersionId}
import js7.data.job.{JobResource, JobResourcePath, ShellScriptExecutable}
import js7.data.lock.{Lock, LockPath, LockState}
import js7.data.node.NodeId
import js7.data.order.Order.ExternalOrderLink
import js7.data.order.{Order, OrderId}
import js7.data.orderwatch.OrderWatchState.{HasOrder, Vanished}
import js7.data.orderwatch.{ExternalOrderName, FileWatch, OrderWatchPath, OrderWatchState}
import js7.data.plan.{PlanId, PlanSchemaId, PlanSchemaState}
import js7.data.subagent.{SubagentBundle, SubagentBundleId, SubagentBundleState, SubagentId, SubagentItem, SubagentItemState}
import js7.data.value.expression.Expression.{NumericConstant, StringConstant}
import js7.data.value.expression.ExpressionParser.expr
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.instructions.{Execute, ExpectNotices, LockInstruction}
import js7.data.workflow.position.{Label, Position}
import js7.data.workflow.{Workflow, WorkflowPath, WorkflowPathControl, WorkflowPathControlPath}
import js7.tester.CirceJsonTester.testJson
import js7.tester.DiffxAssertions.assertEqual

/**
  * @author Joacim Zschimmer
  */
final class ControllerStateTest extends OurAsyncTestSuite:

  private given IORuntime = ioRuntime

  "estimatedSnapshotSize" in:
    assert(controllerState.estimatedSnapshotSize == 21)
    for n <- controllerState.toSnapshotStream.compile.count.unsafeToFuture() yield
      assert(controllerState.estimatedSnapshotSize == n)

  "pathToSimpleItem" in:
    val sum =
      controllerState.keyToUnsignedItemState_.map(_._2.item) ++
        controllerState.pathToSignedSimpleItem.values.map(_.value)
    assert(controllerState.pathToSimpleItem.toMap == sum.toKeyedMap(_.key))

  "toSnapshotStream" in:
    for list <- controllerState.toSnapshotStream.compile.toVector.unsafeToFuture()
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
                ClusterTiming(10.s, 20.s),
                Some(ClusterWatchId("CLUSTER-WATCH"))))),
          controllerState.controllerMetaState,
          WorkflowPathControl(
            WorkflowPathControlPath(workflow.path), suspended = true, skip = Set("LABEL"),
            itemRevision = Some(ItemRevision(0)))
        ) ++
          controllerState.keyTo(AgentRefState).values ++
          controllerState.keyTo(SubagentItemState).values ++
          controllerState.pathToUnsignedSimple(SubagentBundle).values ++
          controllerState.keyTo(LockState).values ++
          Seq(board) ++
          boardState.notices ++
          Seq(calendar) ++
          Seq(
            UnsignedSimpleItemAdded(FileWatch(
              fileWatch.path,
              workflow.path,
              agentRef.path,
              expr("'/tmp/directory'"),
              itemRevision = Some(ItemRevision(7)))),
            OrderWatchState.ExternalOrderSnapshot(
              fileWatch.path,
              ExternalOrderName("ORDER-NAME"),
              HasOrder(OrderId("ORDER"), Some(Vanished))),
            SignedItemAdded(signedJobResource),
            VersionAdded(versionId),
            VersionedItemAdded(signedWorkflow)) ++
          Seq(
            ItemAttachable(jobResource.path, agentRef.path),
            ItemDeletionMarked(fileWatch.path)) ++
          controllerState.idToOrder.values)

  "isWorkflowUsedByOrders" in:
    assert(controllerState.isWorkflowUsedByOrders == Set(workflow.id))

  "isWorkflowUsedByOrders does not include orderless workflows" in:
    val v = VersionId("X")
    val workflowId = WorkflowPath("X") ~ v
    val controllerStateWithOrderlessWorkflow = controllerState
      .applyKeyedEvents(Seq(
        VersionAdded(v),
        VersionedItemAdded(itemSigner.sign(Workflow(workflowId, Nil)))))
      .orThrow
    assert(controllerStateWithOrderlessWorkflow.isWorkflowUsedByOrders == Set(workflow.id))

  "pathToReferencingItemKeys" in:
    val x = controllerState.pathToReferencingItemKeys.mapValuesStrict(_.toSet)
    assert(x == Map[InventoryItemPath, Set[InventoryItemKey]](
      lock.path -> Set(workflow.id),
      board.path -> Set(workflow.id),
      agentRef.path -> Set(subagentItem.id, fileWatch.path, workflow.id),
      subagentItem.id -> Set(agentRef.path, subagentBundle.id),
      subagentBundle.id -> Set(workflow.id),
      jobResource.path -> Set(workflow.id),
      workflow.path -> Set(fileWatch.path)))

  "pathToReferencingItemKeys does not return hidden workflows without orders" in:
    val v = VersionId("x")
    val changedWorkflowId = workflow.path ~ v
    val controllerState = ControllerStateTest.controllerState
      .applyKeyedEvents(Seq(
        VersionAdded(v),
        VersionedItemChanged(itemSigner.sign(workflow.copy(id = changedWorkflowId)))))
      .orThrow
      .copy(idToOrder = Map.empty)
    // The original workflow is still in use by an order and not deleted
    val x = controllerState.pathToReferencingItemKeys.view.mapValues(_.toSet).toMap
    assert(x == Map[InventoryItemPath, Set[InventoryItemKey]](
        lock.path -> Set(workflow.id, changedWorkflowId),
        board.path -> Set(workflow.id, changedWorkflowId),
        agentRef.path -> Set(subagentItem.id, fileWatch.path, workflow.id, changedWorkflowId),
        subagentItem.id -> Set(agentRef.path, subagentBundle.id),
        subagentBundle.id -> Set(workflow.id, changedWorkflowId),
        jobResource.path -> Set(workflow.id, changedWorkflowId),
        workflow.path -> Set(fileWatch.path)))

  "fromIterator is the reverse of toSnapshotStream" in:
    ControllerState
      .fromStream(controllerState.toSnapshotStream)
      .map(expectedState => assertEqual(controllerState, expectedState))
      .unsafeToFuture()

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
            "timing": {
              "heartbeat": 10,
              "heartbeatTimeout": 20
            },
            "clusterWatchId": "CLUSTER-WATCH"
          }
        }
      }, {
        "TYPE": "ControllerMetaState",
        "controllerId": "CONTROLLER-ID",
        "initiallyStartedAt": 1558699200000,
        "timezone": "Europe/Berlin"
      }, {
        "TYPE": "WorkflowPathControl",
        "path": "WORKFLOW",
        "suspended": true,
        "skip": [ "LABEL" ],
        "itemRevision": 0
      }, {
        "TYPE": "AgentRefState",
        "agentRef": {
          "path": "AGENT",
          "directors": [ "SUBAGENT" ],
          "itemRevision": 0
        },
        "couplingState": {
          "TYPE": "Coupled"
        },
        "clusterState": {
          "TYPE": "Empty"
        },
        "eventId": 7
      }, {
        "TYPE": "SubagentItemState",
        "subagentItem": {
          "id": "SUBAGENT",
          "agentPath": "AGENT",
          "uri": "https://SUBAGENT",
          "disabled": false,
          "itemRevision": 7
        },
        "couplingState": {
          "TYPE": "Reset",
          "reason": {
            "TYPE": "Fresh"
          }
        },
        "eventId": 0
      }, {
        "TYPE": "SubagentBundle",
        "id": "BUNDLE",
        "subagentToPriority": {
          "SUBAGENT": 1
        },
        "itemRevision": 7
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
        "TYPE": "GlobalBoard",
        "path": "BOARD",
        "postOrderToNoticeKey": "$$orderId",
        "expectOrderToNoticeKey": "$$orderId",
        "endOfLife": "$$js7EpochMilli + 24 * 3600 * 1000",
        "itemRevision": 7
      }, {
        "TYPE": "Notice",
        "id": [ "BOARD", "NOTICE-1" ],
        "endOfLife": 10086400000
      }, {
        "TYPE": "Calendar",
        "path": "Calendar",
        "dateOffset": 21600,
        "orderIdPattern": "#([^#]+)#.*",
        "periodDatePattern": "yyyy-MM-dd",
        "itemRevision": 1
      }, {
        "TYPE": "UnsignedSimpleItemAdded",
        "item": {
          "TYPE": "FileWatch",
          "path": "WATCH",
          "workflowPath": "WORKFLOW",
          "agentPath": "AGENT",
          "directoryExpr": "'/tmp/directory'",
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
            "TYPE": "Vanished"
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
          "string": "{\"TYPE\":\"Workflow\",\"path\":\"WORKFLOW\",\"versionId\":\"1.0\",\"instructions\":[{\"TYPE\":\"Lock\",\"demands\":[{\"lockPath\":\"LOCK\"}],\"lockedWorkflow\":{\"instructions\":[{\"TYPE\":\"Execute.Anonymous\",\"job\":{\"agentPath\":\"AGENT\",\"subagentBundleIdExpr\":\"'BUNDLE'\",\"executable\":{\"TYPE\":\"ShellScriptExecutable\",\"script\":\"\"},\"jobResourcePaths\":[\"JOB-RESOURCE\"],\"processLimit\":1}}]}},{\"TYPE\":\"ExpectNotices\",\"boardPaths\":\"'BOARD'\",\"whenNotAnnounced\":\"Wait\"}]}"
        }
      }, {
        "TYPE": "ItemAttachable",
        "delegateId": "Agent:AGENT",
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
        "externalOrder": {
          "orderWatchPath": "WATCH",
          "name": "ORDER-NAME",
          "vanished": true
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
          "TYPE": "ExpectingNotices",
          "noticeIds": [
            [ "BOARD", "NOTICE-2" ]
          ]
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

  "toSnapshotStream JSON" in:
    implicit val x = ControllerState.snapshotObjectJsonCodec
    for
      jsonArray <- controllerState.toSnapshotStream.compile.toVector.unsafeToFuture()
      assertion <- testJson(jsonArray, expectedSnapshotJsonArray)
    yield assertion

  "ControllerStateBuilder.addSnapshotObject" in:
    ControllerState.snapshotObjectJsonCodec
    val builder = new ControllerStateBuilder
    expectedSnapshotJsonArray.asArray.get
      .map(json => ControllerState.snapshotObjectJsonCodec.decodeJson(json).toChecked.orThrow)
      .foreach(builder.addSnapshotObject)
    builder.onAllSnapshotsAdded()
    assertEqual(builder.result(), controllerState)

  "keyToItem" in:
    assert(!controllerState.keyToItem.contains(WorkflowPath("UNKNOWN") ~ "1"))

    assert(controllerState.keyToItem.get(jobResource.path) == Some(jobResource))
    assert(controllerState.keyToItem.get(agentRef.path) == Some(agentRef))
    assert(controllerState.keyToItem.get(lock.path) == Some(lock))
    assert(controllerState.keyToItem.get(fileWatch.path) == Some(fileWatch))
    assert(controllerState.keyToItem.get(workflow.id) == Some(workflow))

    assert(controllerState.keyToItem.keySet == Set(
      jobResource.path, calendar.path,
      agentRef.path, subagentItem.id, subagentBundle.id,
      lock.path, board.path, fileWatch.path,
      PlanSchemaId.Global,
      workflow.id, WorkflowPathControlPath(workflow.path)))

  "v2.2 compatiblity" - {
    // COMPATIBLE with v2.2
    var cs = ControllerState.empty
    val builder = ControllerState.newBuilder()
    val eventIds = Iterator.from(1)
    val agentPath = AgentPath("v2.2")
    val uri = Uri("https://localhost")
    val agentRef = AgentRef(agentPath, directors = Nil, Some(uri),
      itemRevision = Some(ItemRevision(7)))

    val subagentId = SubagentId("v2.2-1")
    val generatedSubagentItem = SubagentItem(subagentId, agentPath, uri,
      itemRevision = Some(ItemRevision(1)))

    def applyEvent(event: UnsignedSimpleItemEvent): Unit =
      val eventId = eventIds.next()
      cs = cs.applyKeyedEvent(event).orThrow
      cs = cs.withEventId(eventId)
      builder.addEvent(Stamped(eventId, event))
      assert(builder.result() == cs)

    "UnsignedSimpleItemAdded" in:
      applyEvent(UnsignedSimpleItemAdded(agentRef))

      assert(cs.keyTo(AgentRefState)(agentRef.path).agentRef == agentRef.copy(
        directors = Seq(subagentId),
        uri = None))
      assert(cs.keyTo(SubagentItemState)(subagentId).subagentItem == generatedSubagentItem)

    "AgentRefState snapshot object" in:
      val b = ControllerState.newBuilder()
      b.addSnapshotObject(AgentRefState(agentRef))
      b.onAllSnapshotsAdded()
      val x = b.result()
      assert(b.result() == cs.withEventId(0))

    "UnsignedSimpleItemChanged" in:
      val changedUri = Uri("https://example.com")
      val changedAgentRef = agentRef.copy(
        uri = Some(changedUri),
        itemRevision = Some(ItemRevision(8)))
      applyEvent(UnsignedSimpleItemChanged(changedAgentRef))
      assert(cs.keyTo(AgentRefState)(agentRef.path).agentRef == agentRef.copy(
        directors = Seq(subagentId),
        uri = None,
        itemRevision = Some(ItemRevision(8))))
      assert(cs.keyTo(SubagentItemState)(subagentId).subagentItem == generatedSubagentItem.copy(
        uri = changedUri,
        itemRevision = Some(ItemRevision(2))))
  }


object ControllerStateTest:

  private val jobResource = JobResource(JobResourcePath("JOB-RESOURCE"))
  private lazy val itemSigner = new ItemSigner(SillySigner.Default, ControllerState.signableItemJsonCodec)
  private lazy val signedJobResource = itemSigner.sign(jobResource)
  private val orderId = OrderId("ORDER")
  private val expectingNoticeOrderId = OrderId("ORDER-EXPECTING-NOTICE")
  private val agentRef = AgentRef(AgentPath("AGENT"), directors = Seq(SubagentId("SUBAGENT")),
    itemRevision = Some(ItemRevision(0)))

  private val subagentItem = SubagentItem(
    SubagentId("SUBAGENT"),
    AgentPath("AGENT"),
    Uri("https://SUBAGENT"),
    disabled = false,
    itemRevision = Some(ItemRevision(7)))
  private val subagentItemState = SubagentItemState.initial(subagentItem)
  private val subagentBundle = SubagentBundle(
    SubagentBundleId("BUNDLE"),
    Map(subagentItem.id -> NumericConstant(1)),
    itemRevision = Some(ItemRevision(7)))

  private val lock = Lock(LockPath("LOCK"), itemRevision = Some(ItemRevision(7)))

  private val board = GlobalBoard(
    BoardPath("BOARD"),
    postOrderToNoticeKey = expr("$orderId"),
    expectOrderToNoticeKey = expr("$orderId"),
    endOfLife = expr("$js7EpochMilli + 24*3600*1000"),
    itemRevision = Some(ItemRevision(7)))

  private val notice = Notice(
    board.path / GlobalNoticeKey("NOTICE-1"),
    endOfLife = Timestamp.ofEpochMilli(10_000_000_000L + 24*3600*1000).some)
  private val expectedNoticeId = PlanId.Global / board.path / NoticeKey("NOTICE-2")

  private val boardState = BoardState(
    board,
    Map(
      notice.plannedNoticeKey -> NoticePlace(Some(notice)),
      expectedNoticeId.plannedNoticeKey -> NoticePlace(None, Set(expectingNoticeOrderId))))

  private val calendar = Calendar(
    CalendarPath("Calendar"),
    orderIdToDatePattern = "#([^#]+)#.*",
    periodDatePattern = "yyyy-MM-dd",
    dateOffset = 6.h,
    itemRevision = Some(ItemRevision(1)))

  private val versionId = VersionId("1.0")
  private[controller] val workflow = Workflow(WorkflowPath("WORKFLOW") ~ versionId, Seq(
    LockInstruction.single(lock.path, None, Workflow.of(
      Execute(WorkflowJob(agentRef.path, ShellScriptExecutable(""),
        subagentBundleId = Some(StringConstant(subagentBundle.id.string)),
        jobResourcePaths = Seq(jobResource.path))))),
    ExpectNotices(BoardPathExpression.ExpectNotice(board.path))))
  private val signedWorkflow = itemSigner.sign(workflow)
  private[controller] val fileWatch = FileWatch(
    OrderWatchPath("WATCH"),
    workflow.path,
    AgentPath("AGENT"),
    expr("'/tmp/directory'"),
    itemRevision = Some(ItemRevision(7)))

  // Also used by ControllerStateExecutorTest
  private[controller] lazy val controllerState = ControllerState(
    EventId(1001),
    SnapshotableState.Standards(
      JournalState(Map(UserId("A") -> EventId(1000))),
      ClusterState.Coupled(
        ClusterSetting(
          Map(
            NodeId("A") -> Uri("https://A"),
            NodeId("B") -> Uri("https://B")),
          activeId = NodeId("A"),
          ClusterTiming(10.s, 20.s),
          Some(ClusterWatchId("CLUSTER-WATCH"))))),
    ControllerMetaState(ControllerId("CONTROLLER-ID"), Timestamp("2019-05-24T12:00:00Z"),
      Timezone("Europe/Berlin")),
    Map(
      agentRef.path -> AgentRefState(
        agentRef, None, None, DelegateCouplingState.Coupled, EventId(7), None,
        ClusterState.Empty, Map.empty, None),
      lock.path -> LockState(lock),
      boardState.path -> boardState,
      subagentItem.id -> subagentItemState,
      calendar.path -> CalendarState(calendar),
      subagentBundle.id -> SubagentBundleState(subagentBundle),
      fileWatch.path -> OrderWatchState(
        fileWatch,
        Map(
          ExternalOrderName("ORDER-NAME") -> HasOrder(OrderId("ORDER"), Some(Vanished)))),
      WorkflowPathControlPath(workflow.path) -> WorkflowPathControl(
        WorkflowPathControlPath(workflow.path),
        suspended = true,
        skip = Set(Label("LABEL"))),
      PlanSchemaId.Global -> PlanSchemaState.Global),
    Repo.empty.applyEvents(Seq(
      VersionAdded(versionId),
      VersionedItemAdded(signedWorkflow))).orThrow,
    Map(
      jobResource.path -> signedJobResource),
    ClientAttachments(Map(
      jobResource.path -> Map(agentRef.path -> Attachable))),
    deletionMarkedItems = Set(fileWatch.path),
    Seq(
      Order(orderId, workflow.id /: Position(0), Order.Fresh,
        externalOrder = Some(ExternalOrderLink(
          fileWatch.path,
          ExternalOrderName("ORDER-NAME"),
          vanished = true))),
      Order(expectingNoticeOrderId, workflow.id /: Position(1),
        Order.ExpectingNotices(Vector(
          expectedNoticeId)))
    ).toKeyedMap(_.id)
  ).finish.orThrow
