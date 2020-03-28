package com.sos.jobscheduler.master

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.base.utils.Collections.implicits._
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.core.filebased.Repo
import com.sos.jobscheduler.data.agent.AgentRefPath
import com.sos.jobscheduler.data.cluster.ClusterState
import com.sos.jobscheduler.data.event.EventId
import com.sos.jobscheduler.data.filebased.RepoEvent.VersionAdded
import com.sos.jobscheduler.data.filebased.VersionId
import com.sos.jobscheduler.data.master.{MasterFileBaseds, MasterId}
import com.sos.jobscheduler.data.order.{Order, OrderId}
import com.sos.jobscheduler.data.workflow.WorkflowPath
import com.sos.jobscheduler.data.workflow.position.Position
import com.sos.jobscheduler.master.data.MasterSnapshots
import com.sos.jobscheduler.master.data.MasterSnapshots.MasterMetaState
import com.sos.jobscheduler.master.data.agent.AgentSnapshot
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import monix.execution.Scheduler.Implicits.global
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class MasterStateTest extends FreeSpec
{
  private val masterState = MasterState(
    EventId(1001),
    MasterMetaState(MasterId("MASTER-ID"), Timestamp("2019-05-24T12:00:00Z")),
    ClusterState.Empty,
    Repo(MasterFileBaseds.jsonCodec).applyEvent(VersionAdded(VersionId("1.0"))).orThrow,
    (AgentSnapshot(AgentRefPath("/AGENT"), None, 7) :: Nil).toKeyedMap(_.agentRefPath),
    (Order(OrderId("ORDER"), WorkflowPath("/WORKFLOW") /: Position(1), Order.Fresh(None)) :: Nil).toKeyedMap(_.id))

  //"toSnapshot is equivalent to toSnapshotObservable" in {
  //  assert(masterState.toSnapshots == masterState.toSnapshotObservable.toListL.runToFuture.await(9.s))
  //}

  "fromIterator is the reverse of toSnapshotObservable + EventId" in {
    assert(masterState ==
      MasterState.fromIterator(masterState.toSnapshotObservable.toListL.runToFuture.await(9.s).iterator)
        .copy(eventId = masterState.eventId))
  }

  "toSnapshotObservable JSON" in {
    implicit val x = MasterSnapshots.SnapshotJsonCodec
    testJson(masterState.toSnapshotObservable.toListL.runToFuture.await(9.s),
      json"""[
        {
          "TYPE": "MasterMetaState",
          "masterId": "MASTER-ID",
          "startedAt": 1558699200000
        }, {
          "TYPE": "VersionAdded",
          "versionId": "1.0"
        }, {
          "TYPE": "AgentSnapshot",
          "agentRefPath": "/AGENT",
          "eventId": 7
        }, {
          "TYPE": "Order",
          "historicOutcomes": [],
          "id": "ORDER",
          "state": {
            "TYPE": "Fresh"
          },
          "workflowPosition": {
            "position": [ 1 ],
            "workflowId": {
              "path": "/WORKFLOW"
            }
          }
        }
      ]""")
  }
}
