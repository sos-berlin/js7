package js7.master.data

import js7.base.auth.UserId
import js7.base.circeutils.CirceUtils._
import js7.base.problem.Checked._
import js7.base.time.Timestamp
import js7.base.utils.Collections.implicits._
import js7.base.web.Uri
import js7.data.agent.AgentRefPath
import js7.data.cluster.{ClusterNodeId, ClusterState}
import js7.data.event.{EventId, JournalState, JournaledState}
import js7.data.filebased.RepoEvent.VersionAdded
import js7.data.filebased.{Repo, VersionId}
import js7.data.master.{MasterFileBaseds, MasterId}
import js7.data.order.{Order, OrderId}
import js7.data.workflow.WorkflowPath
import js7.data.workflow.position.Position
import js7.master.data.MasterSnapshots.MasterMetaState
import js7.master.data.agent.AgentSnapshot
import js7.tester.CirceJsonTester.testJson
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
