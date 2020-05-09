package com.sos.jobscheduler.master.data

import com.sos.jobscheduler.base.auth.UserId
import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.base.utils.Collections.implicits._
import com.sos.jobscheduler.base.web.Uri
import com.sos.jobscheduler.data.agent.AgentRefPath
import com.sos.jobscheduler.data.cluster.{ClusterNodeId, ClusterState}
import com.sos.jobscheduler.data.event.{EventId, JournalState, JournaledState}
import com.sos.jobscheduler.data.filebased.RepoEvent.VersionAdded
import com.sos.jobscheduler.data.filebased.{Repo, VersionId}
import com.sos.jobscheduler.data.master.{MasterFileBaseds, MasterId}
import com.sos.jobscheduler.data.order.{Order, OrderId}
import com.sos.jobscheduler.data.workflow.WorkflowPath
import com.sos.jobscheduler.data.workflow.position.Position
import com.sos.jobscheduler.master.data.MasterSnapshots.MasterMetaState
import com.sos.jobscheduler.master.data.agent.AgentSnapshot
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AsyncFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class MasterStateTest extends AsyncFreeSpec
{
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
    Repo.ofJsonDecoder(MasterFileBaseds.jsonCodec).applyEvent(VersionAdded(VersionId("1.0"))).orThrow,
    (AgentSnapshot(AgentRefPath("/AGENT"), None, EventId(7)) :: Nil).toKeyedMap(_.agentRefPath),
    (Order(OrderId("ORDER"), WorkflowPath("/WORKFLOW") /: Position(1), Order.Fresh(None)) :: Nil).toKeyedMap(_.id))

  //"toSnapshot is equivalent to toSnapshotObservable" in {
  //  assert(masterState.toSnapshots == masterState.toSnapshotObservable.toListL.runToFuture.await(9.s))
  //}

  "fromIterator is the reverse of toSnapshotObservable + EventId" in {
    masterState.toSnapshotObservable.toListL.runToFuture
      .map(snapshotObjects =>
        assert(masterState == MasterState.fromIterator(snapshotObjects.iterator).withEventId(masterState.eventId)))
  }

  "toSnapshotObservable JSON" in {
    implicit val x = MasterSnapshots.SnapshotJsonCodec
    masterState.toSnapshotObservable.toListL.runToFuture.map(snapshotObjects =>
      testJson(snapshotObjects,
        json"""[
          {
            "TYPE": "JournalState",
            "userIdToReleasedEventId": {
              "A": 1000
            }
          }, {
            "TYPE": "ClusterStateSnapshot",
            "clusterState": {
              "TYPE": "Coupled",
              "idToUri": {
                "A": "http://A",
                "B": "http://B"
              },
              "activeId": "A"
            }
          }, {
            "TYPE": "MasterMetaState",
            "masterId": "MASTER-ID",
            "startedAt": 1558699200000,
            "timezone": "Europe/Berlin"
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
        ]"""))
  }
}
