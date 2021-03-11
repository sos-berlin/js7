package js7.data.controller

import js7.base.auth.UserId
import js7.base.circeutils.CirceUtils._
import js7.base.problem.Checked._
import js7.base.thread.MonixBlocking.syntax.RichTask
import js7.base.time.ScalaTime._
import js7.base.time.Timestamp
import js7.base.utils.Collections.implicits._
import js7.base.web.Uri
import js7.data.agent.AttachedState.Attached
import js7.data.agent.{AgentId, AgentRef, AgentRefState}
import js7.data.cluster.{ClusterSetting, ClusterState, ClusterStateSnapshot, ClusterTiming}
import js7.data.event.SnapshotMeta.SnapshotEventId
import js7.data.event.{EventId, JournalState, JournaledState}
import js7.data.item.VersionedEvent.VersionAdded
import js7.data.item.{Repo, VersionId}
import js7.data.lock.{Lock, LockId, LockState}
import js7.data.node.NodeId
import js7.data.order.{Order, OrderId}
import js7.data.ordersource.{FileOrderSource, OrderSourceId, OrderSourceState, SourceOrderKey, SourceOrderName}
import js7.data.workflow.WorkflowPath
import js7.data.workflow.position.Position
import js7.tester.CirceJsonTester.testJson
import monix.execution.Scheduler.Implicits.global
import monix.reactive.Observable
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
        ClusterSetting(
          Map(
            NodeId("A") -> Uri("http://A"),
            NodeId("B") -> Uri("http://B")),
          activeId = NodeId("A"),
          Seq(ClusterSetting.Watch(Uri("https://CLUSTER-WATCH"))),
          ClusterTiming(10.s, 20.s)))),
    ControllerMetaState(ControllerId("CONTROLLER-ID"), Timestamp("2019-05-24T12:00:00Z"), timezone = "Europe/Berlin"),
    Map(AgentId("AGENT") ->
      AgentRefState(AgentRef(AgentId("AGENT"), Uri("https://AGENT")), None, None, AgentRefState.Decoupled, EventId(7))),
    Map(LockId("LOCK") ->
      LockState(Lock(LockId("LOCK"), limit = 1))),
    Map(OrderSourceId("SOURCE") ->
      OrderSourceState(
        FileOrderSource(
          OrderSourceId("SOURCE"),
          WorkflowPath("WORKFLOW"),
          AgentId("AGENT"),
          "/tmp/directory"),
        Some(Attached),
        Map(
          SourceOrderName("ORDER-NAME") -> OrderId("ORDER")))),
    Repo.empty.applyEvent(VersionAdded(VersionId("1.0"))).orThrow,
    (Order(OrderId("ORDER"), WorkflowPath("WORKFLOW") /: Position(1), Order.Fresh(None),
      sourceOrderKey = Some(SourceOrderKey(OrderSourceId("SOURCE"), SourceOrderName("ORDER-NAME")))
    ) :: Nil).toKeyedMap(_.id))

  "estimatedSnapshotSize" in {
    assert(controllerState.estimatedSnapshotSize == 9)
    for (list <- controllerState.toSnapshotObservable.toListL.runToFuture)
      yield assert(list.size == controllerState.estimatedSnapshotSize)
    assert(controllerState.estimatedSnapshotSize ==
      controllerState.toSnapshotObservable.countL.await(99.s))
  }

  "idToItem" in {
    val sum = controllerState.idToAgentRefState ++
      controllerState.idToLockState ++
      controllerState.idToOrderSourceState
    assert(controllerState.idToItem.toMap == sum.map(_._2.item).toKeyedMap(_.id))
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
          controllerState.idToAgentRefState.values ++
          controllerState.idToLockState.values ++
          Seq(
            OrderSourceState.HeaderSnapshot(
              FileOrderSource(
                OrderSourceId("SOURCE"),
                WorkflowPath("WORKFLOW"),
                AgentId("AGENT"),
                "/tmp/directory"),
              Some(Attached))) ++
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
              "id": "AGENT",
              "uri": "https://AGENT",
              "itemRevision": 0
            },
            "couplingState": {
              "TYPE": "Decoupled"
            },
            "eventId": 7
          }, {
            "TYPE": "LockState",
            "lock": {
              "id": "LOCK",
              "limit": 1,
              "itemRevision": 0
            },
            "acquired": {
              "TYPE": "Available"
            },
            "queue": []
          }, {
            "TYPE": "OrderSourceState.Snapshot",
            "orderSource": {
              "TYPE": "FileOrderSource",
              "agentId": "AGENT",
              "directory": "/tmp/directory",
              "id": "SOURCE",
              "workflowPath": "WORKFLOW",
              "itemRevision": 0
            },
            "attached": { "TYPE": "Attached" }
          }, {
            "TYPE": "Order",
            "id": "ORDER",
            "state": {
              "TYPE": "Fresh"
            },
            "sourceOrderKey": {
              "orderSourceId": "SOURCE",
              "name": "ORDER-NAME"
            },
            "workflowPosition": {
              "position": [ 1 ],
              "workflowId": {
                "path": "WORKFLOW"
              }
            }
          }
        ]"""))
  }
}
