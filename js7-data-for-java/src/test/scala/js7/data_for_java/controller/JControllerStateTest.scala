package js7.data_for_java.controller

import js7.base.auth.UserId
import js7.base.crypt.silly.SillySigner
import js7.base.problem.Checked.Ops
import js7.base.time.ScalaTime._
import js7.base.time.{Timestamp, Timezone}
import js7.base.utils.Collections.implicits._
import js7.base.web.Uri
import js7.data.agent.{AgentPath, AgentRef, AgentRefState, DelegateCouplingState}
import js7.data.cluster.{ClusterSetting, ClusterState, ClusterTiming}
import js7.data.controller.ControllerState.versionedItemJsonCodec
import js7.data.controller.{ControllerId, ControllerMetaState, ControllerState}
import js7.data.event.{EventId, JournalState, SnapshotableState}
import js7.data.item.VersionedEvent.{VersionAdded, VersionedItemAdded}
import js7.data.item.{ClientAttachments, ItemSigner, Repo, VersionId}
import js7.data.node.NodeId
import js7.data.order.{Order, OrderId}
import js7.data.orderwatch.AllOrderWatchesState
import js7.data.subagent.SubagentId
import js7.data.value.StringValue
import js7.data.workflow.position.Position
import js7.data.workflow.{WorkflowParser, WorkflowPath}
import js7.data_for_java.controller.JControllerStateTest._
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class JControllerStateTest extends AnyFreeSpec
{
  private val jControllerState = JControllerState(controllerState)
  private val tester = new JControllerStateTester(jControllerState)

  "clusterState" in {
    tester.testClusterState()
  }

  "workflow" in {
    tester.testWorkflows()
  }

  "orderIds" in {
    tester.testOrderIds()
  }

  "IdToOrder" in {
    tester.testIdToOrder()
  }

  "eagerIdToOrder" in {
    tester.testEagerIdToOrder()
  }

  "testOrderByWorkflowPath" in {
    tester.testOrdersBy()
  }

  "testStateToOrder" in {
    tester.testOrderStateToCount()
  }
}

private object JControllerStateTest
{
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

  private val controllerState = ControllerState(
    EventId(1001),
    SnapshotableState.Standards(
      JournalState(Map(UserId("A") -> EventId(1000))),
      ClusterState.Coupled(
        ClusterSetting(
          Map(
            NodeId("A") -> Uri("http://A"),
            NodeId("B") -> Uri("http://B")),
          activeId = NodeId("A"),
          Seq(ClusterSetting.Watch(Uri("https://CLUSTER-WATCH"))),
          ClusterTiming(10.s, 20.s)))),
    ControllerMetaState(
      ControllerId("CONTROLLER-ID"),
      Timestamp("2019-05-24T12:00:00Z"),
      Timezone("Europe/Berlin")),
    Map(AgentPath("AGENT") ->
      AgentRefState(
        AgentRef(AgentPath("AGENT"), Seq(SubagentId("SUBAGENT"))),
        None, None, DelegateCouplingState.Reset, EventId(7), None)),
    Map.empty,
    Map.empty,
    Map.empty,
    Map.empty,
    AllOrderWatchesState.empty,
    Repo.empty
      .applyEvents(List(
        VersionAdded(v1),
        VersionedItemAdded(itemSigner.sign(aWorkflow)),
        VersionedItemAdded(itemSigner.sign(bWorkflow)))
      ).orThrow,
    Map.empty,
    ClientAttachments.empty,
    Set.empty,
    Vector(
      Order(
        OrderId("A-ORDER"),
        (WorkflowPath("A-WORKFLOW") ~ v1) /: Position(0),
        Order.Fresh),
      Order(
        OrderId("B-ORDER"),
        (WorkflowPath("B-WORKFLOW") ~ v2) /: Position(0),
        Order.Ready,
        Map(
          "key1" -> StringValue("value1"),
          "key2" -> StringValue("value2")),
        deleteWhenTerminated = true)
    ).toKeyedMap(_.id))
}
