package js7.proxy.javaapi.data.controller

import js7.base.auth.UserId
import js7.base.crypt.silly.SillySigner
import js7.base.problem.Checked.Ops
import js7.base.time.ScalaTime._
import js7.base.time.Timestamp
import js7.base.utils.Collections.implicits._
import js7.base.web.Uri
import js7.controller.data.agent.AgentRefState
import js7.controller.data.{ControllerMetaState, ControllerState}
import js7.data.agent.{AgentId, AgentRef}
import js7.data.cluster.{ClusterSetting, ClusterState, ClusterTiming}
import js7.data.controller.{ControllerId, ControllerItems}
import js7.data.event.{EventId, JournalState, JournaledState}
import js7.data.item.RepoEvent.VersionAdded
import js7.data.item.{Repo, VersionId, VersionedItemSigner}
import js7.data.node.NodeId
import js7.data.order.{Order, OrderId}
import js7.data.value.StringValue
import js7.data.workflow.position.Position
import js7.data.workflow.{WorkflowParser, WorkflowPath}
import js7.proxy.javaapi.data.controller.JControllerStateTest._
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
  private val aWorkflow = WorkflowParser.parse(WorkflowPath("/A-WORKFLOW") ~ v1,
    """|define workflow {
       |  execute agent="AGENT", executable="A-EXECUTABLE";
       |}
       |""".stripMargin).orThrow
  private val bWorkflow = WorkflowParser.parse(WorkflowPath("/B-WORKFLOW") ~ v1,
    """|define workflow {
       |  execute agent="AGENT", executable="B-EXECUTABLE";
       |}
       |""".stripMargin).orThrow

  private val itemSigner = new VersionedItemSigner(SillySigner.Default, ControllerItems.jsonCodec)

  private val controllerState = ControllerState(
    EventId(1001),
    JournaledState.Standards(
      JournalState(Map(UserId("A") -> EventId(1000))),
      ClusterState.Coupled(
        ClusterSetting(
          Map(
            NodeId("A") -> Uri("http://A"),
            NodeId("B") -> Uri("http://B")),
          activeId = NodeId("A"),
          Seq(ClusterSetting.Watch(Uri("https://CLUSTER-WATCH"))),
          ClusterTiming(10.s, 20.s)))),
    ControllerMetaState(ControllerId("CONTROLLER-ID"), Timestamp("2019-05-24T12:00:00Z"), timezone = "Europe/Berlin"),
    (AgentRefState(AgentRef(AgentId("AGENT"), Uri("https://AGENT")), None, None, AgentRefState.Decoupled, EventId(7)) :: Nil).toKeyedMap(_.name),
    Map.empty,
    Repo.empty
      .applyEvents(List(
        VersionAdded(v1),
        itemSigner.toAddedEvent(aWorkflow),
        itemSigner.toAddedEvent(bWorkflow)),
      ).orThrow,
    Vector(
      Order(
        OrderId("A-ORDER"),
        (WorkflowPath("/A-WORKFLOW") ~ v1) /: Position(0),
        Order.Fresh(None)),
      Order(
        OrderId("B-ORDER"),
        (WorkflowPath("/B-WORKFLOW") ~ v2) /: Position(0),
        Order.Ready,
        Map(
          "key1" -> StringValue("value1"),
          "key2" -> StringValue("value2")),
        removeWhenTerminated = true)
    ).toKeyedMap(_.id))
}
