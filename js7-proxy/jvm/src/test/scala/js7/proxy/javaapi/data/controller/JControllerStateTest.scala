package js7.proxy.javaapi.data.controller

import js7.base.annotation.javaApi
import js7.base.auth.UserId
import js7.base.crypt.silly.SillySigner
import js7.base.problem.Checked.Ops
import js7.base.time.Timestamp
import js7.base.utils.Collections.implicits._
import js7.base.web.Uri
import js7.controller.data.{ControllerMetaState, ControllerState}
import js7.controller.data.agent.AgentSnapshot
import js7.data.agent.{AgentRef, AgentRefPath}
import js7.data.cluster.ClusterState
import js7.data.controller.{ControllerId, ControllerItems}
import js7.data.event.{EventId, JournalState, JournaledState}
import js7.data.item.RepoEvent.VersionAdded
import js7.data.item.{InventoryItemSigner, Repo, VersionId}
import js7.data.node.NodeId
import js7.data.order.{Order, OrderId}
import js7.data.workflow.WorkflowPath
import js7.data.workflow.parser.WorkflowParser
import js7.data.workflow.position.Position
import js7.proxy.javaapi.data.controller.JControllerStateTest._
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
@javaApi
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
  private val versionId = VersionId("VERSION-ID")
  private val aWorkflow = WorkflowParser.parse(WorkflowPath("/A-WORKFLOW") ~ versionId,
    """|define workflow {
       |  execute agent="/AGENT", executable="/A-EXECUTABLE";
       |}
       |""".stripMargin).orThrow
  private val bWorkflow = WorkflowParser.parse(WorkflowPath("/B-WORKFLOW") ~ versionId,
    """|define workflow {
       |  execute agent="/AGENT", executable="/B-EXECUTABLE";
       |}
       |""".stripMargin).orThrow
  private val agentRef = AgentRef(AgentRefPath("/AGENT") ~ versionId, Uri("http://agent.example.com"))

  private val itemSigner = new InventoryItemSigner(SillySigner.Default, ControllerItems.jsonCodec)

  private val controllerState = ControllerState(
    EventId(1001),
    JournaledState.Standards(
      JournalState(Map(UserId("A") -> EventId(1000))),
      ClusterState.Coupled(
        Map(
          NodeId("A") -> Uri("http://A"),
          NodeId("B") -> Uri("http://B")),
        NodeId("A"))),
    ControllerMetaState(ControllerId("CONTROLLER-ID"), Timestamp("2019-05-24T12:00:00Z"), timezone = "Europe/Berlin"),
    Repo.empty
      .applyEvents(List(
        VersionAdded(versionId),
        itemSigner.toAddedEvent(agentRef),
        itemSigner.toAddedEvent(aWorkflow),
        itemSigner.toAddedEvent(bWorkflow)),
      ).orThrow,
    (AgentSnapshot(AgentRefPath("/AGENT"), None, EventId(7)) :: Nil).toKeyedMap(_.agentRefPath),
    Vector(
      Order(
        OrderId("A-ORDER"),
        (WorkflowPath("/A-WORKFLOW") ~ "VERSION-ID") /: Position(0),
        Order.Fresh(None)),
      Order(
        OrderId("B-ORDER"),
        (WorkflowPath("/B-WORKFLOW") ~ "VERSION-ID") /: Position(0),
        Order.Ready,
        Map(
          "key1" -> "value1",
          "key2" -> "value2"))
    ).toKeyedMap(_.id))
}
