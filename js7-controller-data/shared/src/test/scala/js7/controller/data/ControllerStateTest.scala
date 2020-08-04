package js7.controller.data

import js7.base.auth.UserId
import js7.base.circeutils.CirceUtils._
import js7.base.problem.Checked._
import js7.base.time.Timestamp
import js7.base.utils.Collections.implicits._
import js7.base.web.Uri
import js7.controller.data.ControllerSnapshots.ControllerMetaState
import js7.controller.data.agent.AgentSnapshot
import js7.data.agent.AgentRefPath
import js7.data.cluster.ClusterState
import js7.data.cluster.ClusterState.ClusterStateSnapshot
import js7.data.controller.{ControllerId, ControllerItems}
import js7.data.event.SnapshotMeta.SnapshotEventId
import js7.data.event.{EventId, JournalState, JournaledState}
import js7.data.item.RepoEvent.VersionAdded
import js7.data.item.{Repo, VersionId}
import js7.data.node.NodeId
import js7.data.order.{Order, OrderId}
import js7.data.workflow.WorkflowPath
import js7.data.workflow.position.Position
import js7.tester.CirceJsonTester.testJson
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AsyncFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class ControllerStateTest extends AsyncFreeSpec
{
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
    Repo.ofJsonDecoder(ControllerItems.jsonCodec).applyEvent(VersionAdded(VersionId("1.0"))).orThrow,
    (AgentSnapshot(AgentRefPath("/AGENT"), None, EventId(7)) :: Nil).toKeyedMap(_.agentRefPath),
    (Order(OrderId("ORDER"), WorkflowPath("/WORKFLOW") /: Position(1), Order.Fresh(None)) :: Nil).toKeyedMap(_.id))

  //"toSnapshot is equivalent to toSnapshotObservable" in {
  //  assert(controllerState.toSnapshots == controllerState.toSnapshotObservable.toListL.runToFuture.await(9.s))
  //}

  "estimatedSnapshotSize" in {
    assert(controllerState.estimatedSnapshotSize == 7)
    for (list <- controllerState.toSnapshotObservable.toListL.runToFuture)
      yield assert(list.size == controllerState.estimatedSnapshotSize)
  }

  "toSnapshotObservable" in {
    for (list <- controllerState.toSnapshotObservable.toListL.runToFuture)
      yield assert(list ==
        List(
          SnapshotEventId(1001L),
          JournalState(Map(UserId("A") -> EventId(1000))),
          ClusterStateSnapshot(
            ClusterState.Coupled(
              Map(
                NodeId("A") -> Uri("http://A"),
                NodeId("B") -> Uri("http://B")),
              NodeId("A"))),
          controllerState.controllerMetaState,
          VersionAdded(VersionId("1.0"))
        ) ++
          controllerState.pathToAgentSnapshot.values ++
          controllerState.idToOrder.values)
  }

  "fromIterator is the reverse of toSnapshotObservable + EventId" in {
    controllerState.toSnapshotObservable.toListL.runToFuture
      .map(snapshotObjects =>
        assert(controllerState == ControllerState.fromIterator(snapshotObjects.iterator)))
  }

  "toSnapshotObservable JSON" in {
    implicit val x = ControllerSnapshots.SnapshotJsonCodec
    controllerState.toSnapshotObservable.toListL.runToFuture.map(snapshotObjects =>
      testJson(snapshotObjects,
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
              "idToUri": {
                "A": "http://A",
                "B": "http://B"
              },
              "activeId": "A"
            }
          }, {
            "TYPE": "ControllerMetaState",
            "controllerId": "CONTROLLER-ID",
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
