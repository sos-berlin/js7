package js7.data.controller

import js7.base.auth.UserId
import js7.base.circeutils.CirceUtils._
import js7.base.crypt.silly.SillySigner
import js7.base.problem.Checked._
import js7.base.time.ScalaTime._
import js7.base.time.Timestamp
import js7.base.utils.Collections.implicits._
import js7.base.web.Uri
import js7.data.agent.{AgentPath, AgentRef, AgentRefState}
import js7.data.cluster.{ClusterSetting, ClusterState, ClusterStateSnapshot, ClusterTiming}
import js7.data.controller.ControllerState.ItemAttachedStateSnapshot
import js7.data.event.SnapshotMeta.SnapshotEventId
import js7.data.event.{EventId, JournalState, JournaledState}
import js7.data.item.ItemAttachedState.{Attachable, Attached}
import js7.data.item.SignedItemEvent.SignedItemAdded
import js7.data.item.VersionedEvent.VersionAdded
import js7.data.item.{ItemRevision, ItemSigner, Repo, VersionId}
import js7.data.job.{JobResource, JobResourcePath}
import js7.data.lock.{Lock, LockPath, LockState}
import js7.data.node.NodeId
import js7.data.order.{Order, OrderId}
import js7.data.orderwatch.OrderWatchState.{HasOrder, VanishedAck}
import js7.data.orderwatch.{AllOrderWatchesState, ExternalOrderKey, ExternalOrderName, FileWatch, OrderWatchPath, OrderWatchState}
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
  private lazy val jobResource = JobResource(JobResourcePath("JOB-RESOURCE"))
  private lazy val signedJobResource = new ItemSigner(SillySigner.Default, ControllerState.signableSimpleItemJsonCodec)
    .sign(jobResource)

  private lazy val controllerState = ControllerState(
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
    Map(
      AgentPath("AGENT") -> AgentRefState(
        AgentRef(AgentPath("AGENT"), Uri("https://AGENT"), Some(ItemRevision(0))),
        None, None, AgentRefState.Decoupled, EventId(7))),
    Map(
      LockPath("LOCK") -> LockState(Lock(LockPath("LOCK"), limit = 1, Some(ItemRevision(7))))),
    AllOrderWatchesState(Map(
      OrderWatchPath("WATCH") -> OrderWatchState(
        FileWatch(
          OrderWatchPath("WATCH"),
          WorkflowPath("WORKFLOW"),
          AgentPath("AGENT"),
          "/tmp/directory",
          itemRevision = Some(ItemRevision(7))),
        Map(AgentPath("AGENT") -> Attached(Some(ItemRevision(7)))),
        Map(
          ExternalOrderName("ORDER-NAME") -> HasOrder(OrderId("ORDER"), Some(VanishedAck)))))),
    Repo.empty.applyEvent(VersionAdded(VersionId("1.0"))).orThrow,
    Map(
      jobResource.path -> signedJobResource),
    Map(
      JobResourcePath("JOB-RESOURCE") -> Map(AgentPath("AGENT") -> Attachable)),
    (Order(OrderId("ORDER"), WorkflowPath("WORKFLOW") /: Position(1), Order.Fresh,
      externalOrderKey = Some(ExternalOrderKey(OrderWatchPath("WATCH"), ExternalOrderName("ORDER-NAME")))
    ) :: Nil).toKeyedMap(_.id))

  "estimatedSnapshotSize" in {
    assert(controllerState.estimatedSnapshotSize == 12)
    for (n <- controllerState.toSnapshotObservable.countL.runToFuture) yield
      assert(controllerState.estimatedSnapshotSize == n)
  }

  "pathToSimpleItem" in {
    val sum = controllerState.pathToAgentRefState ++
      controllerState.pathToLockState ++
      controllerState.allOrderWatchesState.pathToOrderWatchState
    assert(controllerState.pathToSimpleItem.toMap == sum.map(_._2.item).toKeyedMap(_.key))
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
          controllerState.pathToAgentRefState.values ++
          controllerState.pathToLockState.values ++
          Seq(
            OrderWatchState.HeaderSnapshot(
              FileWatch(
                OrderWatchPath("WATCH"),
                WorkflowPath("WORKFLOW"),
                AgentPath("AGENT"),
                "/tmp/directory",
                itemRevision = Some(ItemRevision(7))),
              Map(AgentPath("AGENT") -> Attached(Some(ItemRevision(7)))),
              delete = false),
            OrderWatchState.ExternalOrderSnapshot(
              OrderWatchPath("WATCH"),
              ExternalOrderName("ORDER-NAME"),
              HasOrder(OrderId("ORDER"), Some(VanishedAck))),
            SignedItemAdded(signedJobResource),
            ItemAttachedStateSnapshot(jobResource.path, Map(AgentPath("AGENT") -> Attachable))
          ) ++
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
              "path": "AGENT",
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
              "path": "LOCK",
              "limit": 1,
              "itemRevision": 7
            },
            "acquired": {
              "TYPE": "Available"
            },
            "queue": []
          }, {
            "TYPE": "OrderWatchState.Header",
            "orderWatch": {
              "TYPE": "FileWatch",
              "path": "WATCH",
              "workflowPath": "WORKFLOW",
              "agentPath": "AGENT",
              "directory": "/tmp/directory",
              "delay": 0,
              "itemRevision": 7
            },
            "agentPathToAttachedState": {
              "AGENT": {
                "TYPE": "Attached",
                "itemRevision": 7
               }
            },
            "delete": false
          }, {
            "TYPE": "ExternalOrder",
            "orderWatchPath": "WATCH",
            "externalOrderName": "ORDER-NAME",
            "state": {
              "TYPE": "HasOrder",
              "orderId": "ORDER",
              "queued": {
                "TYPE": "VanishedAck"
              }
            }
          }, {
            "TYPE": "SignedItemAdded",
            "signed": {
              "string": "{\"TYPE\":\"JobResource\",\"path\":\"JOB-RESOURCE\",\"env\":{}}",
              "signature": {
                "TYPE": "Silly",
                "signatureString": "SILLY-SIGNATURE"
              }
            }
          }, {
            "TYPE": "ItemAttachedStateSnapshot",
            "agentToAttachedState": {
              "AGENT": {
                "TYPE": "Attachable"
              }
            },
            "key": "JobResource:JOB-RESOURCE"
          },
          {
            "TYPE": "Order",
            "id": "ORDER",
            "state": {
              "TYPE": "Fresh"
            },
            "externalOrderKey": {
              "orderWatchPath": "WATCH",
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
