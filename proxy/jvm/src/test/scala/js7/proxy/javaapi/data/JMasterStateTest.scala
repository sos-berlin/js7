package js7.proxy.javaapi.data

import js7.base.annotation.javaApi
import js7.base.auth.UserId
import js7.base.crypt.silly.SillySigner
import js7.base.problem.Checked.Ops
import js7.base.time.Timestamp
import js7.base.utils.Collections.implicits._
import js7.base.web.Uri
import js7.data.agent.{AgentRef, AgentRefPath}
import js7.data.cluster.{ClusterNodeId, ClusterState}
import js7.data.event.{EventId, JournalState, JournaledState}
import js7.data.filebased.RepoEvent.VersionAdded
import js7.data.filebased.{FileBasedSigner, Repo, VersionId}
import js7.data.master.{MasterFileBaseds, MasterId}
import js7.data.order.{Order, OrderId}
import js7.data.workflow.WorkflowPath
import js7.data.workflow.parser.WorkflowParser
import js7.data.workflow.position.Position
import js7.master.data.MasterSnapshots.MasterMetaState
import js7.master.data.MasterState
import js7.master.data.agent.AgentSnapshot
import js7.proxy.javaapi.data.JMasterStateTest._
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
@javaApi
final class JMasterStateTest extends AnyFreeSpec
{
  private val jMasterState = new JMasterState(masterState)
  private val tester = new JMasterStateTester(jMasterState)

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
}

private object JMasterStateTest
{
  private val versionId = VersionId("COMMIT-ID")
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

  private val fileBasedSigner = new FileBasedSigner(SillySigner.Default, MasterFileBaseds.jsonCodec)

  private val masterState = MasterState(
    EventId(1001),
    JournaledState.Standards(
      JournalState(Map(UserId("A") -> EventId(1000))),
      ClusterState.Coupled(
        Map(
          ClusterNodeId("A") -> Uri("http://A"),
          ClusterNodeId("B") -> Uri("http://B")),
        ClusterNodeId("A"))),
    MasterMetaState(MasterId("MASTER-ID"), Timestamp("2019-05-24T12:00:00Z"), timezone = "Europe/Berlin"),
    Repo.ofJsonDecoder(MasterFileBaseds.jsonCodec)
      .applyEvents(List(
        VersionAdded(versionId),
        fileBasedSigner.toAddedEvent(agentRef),
        fileBasedSigner.toAddedEvent(aWorkflow),
        fileBasedSigner.toAddedEvent(bWorkflow)),
      ).orThrow,
    (AgentSnapshot(AgentRefPath("/AGENT"), None, EventId(7)) :: Nil).toKeyedMap(_.agentRefPath),
    Vector(
      Order(
        OrderId("A-ORDER"),
        (WorkflowPath("/A-WORKFLOW") ~ "COMMIT-ID") /: Position(0),
        Order.Fresh(None)),
      Order(
        OrderId("B-ORDER"),
        (WorkflowPath("/B-WORKFLOW") ~ "COMMIT-ID") /: Position(0),
        Order.Ready,
        Map(
          "key1" -> "value1",
          "key2" -> "value2"))
    ).toKeyedMap(_.id))
}
