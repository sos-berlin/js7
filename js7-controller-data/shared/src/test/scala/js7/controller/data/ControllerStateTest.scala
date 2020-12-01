package js7.controller.data

import js7.base.auth.UserId
import js7.base.circeutils.CirceUtils._
import js7.base.problem.Checked._
import js7.base.time.ScalaTime._
import js7.base.time.Timestamp
import js7.base.utils.Collections.implicits._
import js7.base.web.Uri
import js7.controller.data.agent.AgentRefState
import js7.data.agent.{AgentName, AgentRef}
import js7.data.cluster.{ClusterSetting, ClusterState, ClusterStateSnapshot, ClusterTiming}
import js7.data.controller.ControllerId
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
import monix.reactive.Observable
import org.scalatest.freespec.AsyncFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class ControllerStateTest extends AsyncFreeSpec {
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
    (AgentRefState(AgentRef(AgentName("AGENT"), Uri("https://AGENT")), None, None, AgentRefState.Decoupled, EventId(7)) :: Nil)
      .toKeyedMap(_.name),
    Repo.empty.applyEvent(VersionAdded(VersionId("1.0"))).orThrow,
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
              ClusterSetting(
                Map(
                  NodeId("A") -> Uri("http://A"),
                  NodeId("B") -> Uri("http://B")),
                activeId = NodeId("A"),
                Seq(ClusterSetting.Watch(Uri("https://CLUSTER-WATCH"))),
                ClusterTiming(10.s, 20.s)))),
          controllerState.controllerMetaState,
          VersionAdded(VersionId("1.0"))
        ) ++
          controllerState.nameToAgent.values ++
          controllerState.idToOrder.values)
  }

  "fromIterator is the reverse of toSnapshotObservable + EventId" in {
    controllerState.toSnapshotObservable.toListL
      .flatMap(snapshotObjects =>
        ControllerState.fromObservable(Observable.fromIterable(snapshotObjects)))
      .map(expectedState =>
        assert(controllerState == expectedState))
      .runToFuture
  }

  "toSnapshotObservable JSON" in {
    implicit val x = ControllerState.snapshotObjectJsonCodec
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
              "setting": {
                "idToUri": {
                  "A": "http://A",
                  "B": "http://B"
                },
                "activeId": "A",
                "clusterWatches": [ { "uri": "https://CLUSTER-WATCH" } ],
                "timing": {
                  "heartbeat": 10,
                  "heartbeatTimeout": 20
                }
              }
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
            "TYPE": "AgentRefState",
            "agentRef": {
              "name": "AGENT",
              "uri": "https://AGENT"
            },
            "couplingState": {
              "TYPE": "Decoupled"
            },
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
