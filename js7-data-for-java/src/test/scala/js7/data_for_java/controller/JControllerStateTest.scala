package js7.data_for_java.controller

import js7.base.auth.UserId
import js7.base.crypt.silly.SillySigner
import js7.base.problem.Checked.Ops
import js7.base.test.OurTestSuite
import js7.base.time.ScalaTime.*
import js7.base.time.TimestampForTests.ts
import js7.base.time.{Timestamp, Timezone}
import js7.base.utils.Collections.implicits.*
import js7.base.web.Uri
import js7.data.agent.{AgentPath, AgentRef, AgentRefState}
import js7.data.board.{BoardPath, BoardState, NoticeKey, NoticePlace, PlannableBoard, PlannedBoard}
import js7.data.cluster.{ClusterSetting, ClusterState, ClusterTiming}
import js7.data.controller.ControllerState.versionedItemJsonCodec
import js7.data.controller.{ControllerId, ControllerMetaState, ControllerState}
import js7.data.delegate.DelegateCouplingState
import js7.data.event.{EventId, JournalState, SnapshotableState}
import js7.data.item.VersionedEvent.{VersionAdded, VersionedItemAdded}
import js7.data.item.{ItemSigner, Repo, VersionId}
import js7.data.node.NodeId
import js7.data.order.{Order, OrderId}
import js7.data.plan.{Plan, PlanSchema, PlanSchemaId, PlanSchemaState, PlanStatus}
import js7.data.subagent.SubagentId
import js7.data.value.StringValue
import js7.data.workflow.position.Position
import js7.data.workflow.{WorkflowParser, WorkflowPath}
import js7.data_for_java.controller.JControllerStateTest.*

/**
  * @author Joacim Zschimmer
  */
final class JControllerStateTest extends OurTestSuite:
  private val jControllerState = JControllerState(controllerState)
  private val tester = new JControllerStateTester(jControllerState)

  "clusterState" in:
    tester.testClusterState()

  "workflow" in:
    tester.testWorkflows()

  "orderIds" in:
    tester.testOrderIds()

  "IdToOrder" in:
    tester.testIdToOrder()

  "testOrderByWorkflowPath" in:
    tester.testOrdersBy()

  "testStateToOrder" in:
    tester.testOrderStateToCount()

  "toPlan" in:
    tester.testToPlan()


private object JControllerStateTest:
  private val v1 = VersionId("1.0")
  private val v2 = VersionId("2.0")
  private val aWorkflow = WorkflowParser.parse(WorkflowPath("A-WORKFLOW") ~ v1,
    """|define workflow {
       |  execute agent='AGENT', executable='A-EXECUTABLE';
       |}
       |""".stripMargin).orThrow
  private val bWorkflow = WorkflowParser.parse(WorkflowPath("B-WORKFLOW") ~ v1,
    """|define workflow {
       |  execute agent='AGENT', executable='B-EXECUTABLE';
       |}
       |""".stripMargin).orThrow

  private val itemSigner = new ItemSigner(SillySigner.Default, versionedItemJsonCodec)

  private val planId = PlanSchemaId("DailyPlan") / "2025-01-29"
  private val controllerState = ControllerState.empty.copy(
    eventId = EventId(1001),
    standards = SnapshotableState.Standards(
      JournalState(Map(UserId("A") -> EventId(1000))),
      ClusterState.Coupled(
        ClusterSetting(
          Map(
            NodeId("A") -> Uri("https://A"),
            NodeId("B") -> Uri("https://B")),
          activeId = NodeId("A"),
          ClusterTiming(10.s, 20.s),
          clusterWatchId = None))),
    controllerMetaState = ControllerMetaState(
      ControllerId("CONTROLLER-ID"),
      ts"2019-05-24T12:00:00Z",
      Timezone("Europe/Berlin")),
    keyToUnsignedItemState_ = ControllerState.empty.keyToUnsignedItemState_ ++
      Seq(
        AgentRefState(
          AgentRef(AgentPath("AGENT"), Seq(SubagentId("SUBAGENT"))),
          None, None, DelegateCouplingState.Reset.fresh, EventId(7), None,
          ClusterState.Empty, Map.empty, None),
        PlanSchemaState(
          PlanSchema(PlanSchemaId("DailyPlan"), PlanSchema.EachUnknownPlanIsClosed),
          namedValues = Map.empty,
          finishedPlanRetentionPeriod = 3600.s,
          toPlan = Map:
            planId.planKey -> Plan(
              planId,
              PlanStatus.Open,
              orderIds = Set.empty,
              toPlannedBoard = Map(
                BoardPath("BOARD") -> PlannedBoard(
                  planId / BoardPath("BOARD"),
                  toNoticePlace = Map:
                    NoticeKey("NOTICE") -> NoticePlace(isAnnounced = true)),
                BoardPath("BOARD-2") -> PlannedBoard(
                  planId / BoardPath("BOARD-2"),
                  toNoticePlace = Map:
                    NoticeKey("NOTICE") -> NoticePlace(expectingOrderIds = Set(OrderId("B-ORDER"))))))),
        BoardState(
          PlannableBoard(BoardPath("BOARD"))),
        BoardState(
          PlannableBoard(BoardPath("BOARD-2"))),
      ).map(o => o.path -> o),
    repo = Repo.empty
      .applyEvents(List(
        VersionAdded(v1),
        VersionedItemAdded(itemSigner.sign(aWorkflow)),
        VersionedItemAdded(itemSigner.sign(bWorkflow)))
      ).orThrow,
    idToOrder = Vector(
      Order(
        OrderId("A-ORDER"),
        (WorkflowPath("A-WORKFLOW") ~ v1) /: Position(0),
        Order.Fresh()),
      Order(
        OrderId("B-ORDER"),
        (WorkflowPath("B-WORKFLOW") ~ v2) /: Position(0),
        Order.Ready(),
        arguments = Map(
          "key1" -> StringValue("value1"),
          "key2" -> StringValue("value2")),
        planId = planId,
        deleteWhenTerminated = true)
    ).toKeyedMap(_.id))
    .finish.orThrow
