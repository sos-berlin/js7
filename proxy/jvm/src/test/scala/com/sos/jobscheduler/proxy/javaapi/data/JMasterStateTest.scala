package com.sos.jobscheduler.proxy.javaapi.data

import com.sos.jobscheduler.base.annotation.javaApi
import com.sos.jobscheduler.base.auth.UserId
import com.sos.jobscheduler.base.crypt.silly.SillySigner
import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.base.utils.Collections.implicits._
import com.sos.jobscheduler.base.web.Uri
import com.sos.jobscheduler.data.agent.{AgentRef, AgentRefPath}
import com.sos.jobscheduler.data.cluster.{ClusterNodeId, ClusterState}
import com.sos.jobscheduler.data.event.{EventId, JournalState, JournaledState}
import com.sos.jobscheduler.data.filebased.RepoEvent.VersionAdded
import com.sos.jobscheduler.data.filebased.{FileBasedSigner, Repo, VersionId}
import com.sos.jobscheduler.data.master.{MasterFileBaseds, MasterId}
import com.sos.jobscheduler.data.order.{Order, OrderId}
import com.sos.jobscheduler.data.workflow.WorkflowPath
import com.sos.jobscheduler.data.workflow.parser.WorkflowParser
import com.sos.jobscheduler.data.workflow.position.Position
import com.sos.jobscheduler.master.data.MasterSnapshots.MasterMetaState
import com.sos.jobscheduler.master.data.MasterState
import com.sos.jobscheduler.master.data.agent.AgentSnapshot
import com.sos.jobscheduler.proxy.javaapi.data.JMasterStateTest._
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
