package js7.data.controller

import com.softwaremill.diffx.generic.auto._
import js7.base.auth.UserId
import js7.base.circeutils.CirceUtils._
import js7.base.crypt.silly.SillySigner
import js7.base.problem.Checked._
import js7.base.problem.Problem
import js7.base.time.ScalaTime._
import js7.base.time.Timestamp
import js7.base.utils.Collections.implicits._
import js7.base.web.Uri
import js7.data.Problems.{ItemIsStillReferencedProblem, MissingReferencedItemProblem}
import js7.data.agent.{AgentPath, AgentRef, AgentRefState}
import js7.data.cluster.{ClusterSetting, ClusterState, ClusterStateSnapshot, ClusterTiming}
import js7.data.controller.ControllerStateTest._
import js7.data.event.SnapshotMeta.SnapshotEventId
import js7.data.event.{EventId, JournalState, JournaledState}
import js7.data.item.BasicItemEvent.{ItemAttachable, ItemDestroyed, ItemDestructionMarked}
import js7.data.item.ItemAttachedState.{Attachable, Attached}
import js7.data.item.SignedItemEvent.SignedItemAdded
import js7.data.item.UnsignedSimpleItemEvent.UnsignedSimpleItemAdded
import js7.data.item.VersionedEvent.{VersionAdded, VersionedItemAdded, VersionedItemDeleted}
import js7.data.item.{ItemRevision, ItemSigner, Repo, VersionId}
import js7.data.job.{JobResource, JobResourcePath, ScriptExecutable}
import js7.data.lock.{Lock, LockPath, LockState}
import js7.data.node.NodeId
import js7.data.order.OrderEvent.{OrderCancelled, OrderRemoved}
import js7.data.order.{Order, OrderId}
import js7.data.orderwatch.OrderWatchState.{HasOrder, VanishedAck}
import js7.data.orderwatch.{AllOrderWatchesState, ExternalOrderKey, ExternalOrderName, FileWatch, OrderWatchPath, OrderWatchState}
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.instructions.{Execute, LockInstruction}
import js7.data.workflow.position.Position
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tester.CirceJsonTester.testJson
import js7.tester.DiffxAssertions.assertEqual
import monix.execution.Scheduler.Implicits.global
import monix.reactive.Observable
import org.scalatest.freespec.AsyncFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class ControllerStateTest extends AsyncFreeSpec
{
  private lazy val controllerState = ControllerState(
    EventId(1001),
    JournaledState.Standards(
      JournalState(Map(UserId("A") -> EventId(1000))),
      ClusterState.Coupled(
        ClusterSetting(
          Map(
            NodeId("A") -> Uri("https://A"),
            NodeId("B") -> Uri("https://B")),
          activeId = NodeId("A"),
          Seq(ClusterSetting.Watch(Uri("https://CLUSTER-WATCH"))),
          ClusterTiming(10.s, 20.s)))),
    ControllerMetaState(ControllerId("CONTROLLER-ID"), Timestamp("2019-05-24T12:00:00Z"), timezone = "Europe/Berlin"),
    Map(
      agentRef.path -> AgentRefState(
        agentRef, None, None, AgentRefState.Reset, EventId(7))),
    Map(
      lock.path -> LockState(lock)),
    AllOrderWatchesState(Map(
      fileWatch.path -> OrderWatchState(
        fileWatch,
        Map(agentRef.path -> Attached(Some(ItemRevision(7)))),
        Map(
          ExternalOrderName("ORDER-NAME") -> HasOrder(OrderId("ORDER"), Some(VanishedAck)))))),
    Repo.empty.applyEvents(Seq(VersionAdded(versionId), VersionedItemAdded(signedWorkflow))).orThrow,
    Map(
      jobResource.path -> signedJobResource),
    Map(
      jobResource.path -> Map(agentRef.path -> Attachable)),
    destructionMarkedItems = Set(fileWatch.path),
    (Order(orderId, workflow.id /: Position(1), Order.Fresh,
      externalOrderKey = Some(ExternalOrderKey(fileWatch.path, ExternalOrderName("ORDER-NAME")))
    ) :: Nil).toKeyedMap(_.id))

  "estimatedSnapshotSize" in {
    assert(controllerState.estimatedSnapshotSize == 14)
    for (n <- controllerState.toSnapshotObservable.countL.runToFuture) yield
      assert(controllerState.estimatedSnapshotSize == n)
  }

  "pathToSimpleItem" in {
    val sum = (controllerState.pathToAgentRefState ++
      controllerState.pathToLockState ++
      controllerState.allOrderWatchesState.pathToOrderWatchState).map(_._2.item) ++
      controllerState.idToSignedSimpleItem.values.map(_.value)
    assert(controllerState.pathToSimpleItem.toMap == sum.toKeyedMap(_.key))
  }

  "toSnapshotObservable" in {
    for (list <- controllerState.toSnapshotObservable.toListL.runToFuture)
      yield assert(list ==
        Seq(
          SnapshotEventId(1001L),
          JournalState(Map(UserId("A") -> EventId(1000))),
          ClusterStateSnapshot(
            ClusterState.Coupled(
              ClusterSetting(
                Map(
                  NodeId("A") -> Uri("https://A"),
                  NodeId("B") -> Uri("https://B")),
                activeId = NodeId("A"),
                Seq(ClusterSetting.Watch(Uri("https://CLUSTER-WATCH"))),
                ClusterTiming(10.s, 20.s)))),
          controllerState.controllerMetaState
        ) ++
          controllerState.pathToAgentRefState.values ++
          controllerState.pathToLockState.values ++
          Seq(
            UnsignedSimpleItemAdded(FileWatch(
              fileWatch.path,
              workflow.path,
              agentRef.path,
              "/tmp/directory",
              itemRevision = Some(ItemRevision(7)))),
            OrderWatchState.ExternalOrderSnapshot(
              fileWatch.path,
              ExternalOrderName("ORDER-NAME"),
              HasOrder(OrderId("ORDER"), Some(VanishedAck))),
            SignedItemAdded(signedJobResource),
            VersionAdded(versionId),
            VersionedItemAdded(signedWorkflow),
            ItemAttachable(jobResource.path, agentRef.path),
            ItemDestructionMarked(fileWatch.path)
          ) ++
          controllerState.idToOrder.values)
  }

  "checkConsistencyForItems" in {
    assert(controllerState.checkConsistencyForItems(Nil) == Right(()))
    assert(controllerState.checkConsistencyForItems(Seq(fileWatch.path)) == Right(()))
    assert(controllerState
      .applyEvent(VersionedItemDeleted(workflow.path)).orThrow
      .checkConsistencyForItems(Seq(fileWatch.path))
      == Left(MissingReferencedItemProblem(fileWatch.path, workflow.path)))
    assert(controllerState.checkConsistencyForItems(Seq(agentRef.path)) == Right(()))
    assert(controllerState.checkConsistencyForItems(Seq(jobResource.path)) == Right(()))
  }

  "checkConsistencyForDeletedItems" in {
    assert(controllerState.checkConsistencyForDeletedItems(Nil) == Right(()))
    assert(controllerState.checkConsistencyForDeletedItems(Seq(fileWatch.path)) == Right(()))

    assert(controllerState.checkConsistencyForDeletedItems(Seq(workflow.id)) ==
      Left(ItemIsStillReferencedProblem(workflow.id, fileWatch.path)))

    // Destroy Lock
    assert(controllerState.checkConsistencyForDeletedItems(Seq(lock.path))
      == Left(ItemIsStillReferencedProblem(lock.path, workflow.id)))
    assert(controllerState
      .applyEvents(Seq(
        ItemDestroyed(lock.path),
        VersionedItemDeleted(workflow.path))).orThrow
      .checkConsistencyForDeletedItems(Seq(lock.path))
      == Left(ItemIsStillReferencedProblem(lock.path, workflow.id)))

    val myControllerState = controllerState.applyEvents(Seq(
      orderId <-: OrderCancelled,
      orderId <-: OrderRemoved
    )).orThrow

    myControllerState
      .applyEvents(Seq(
        VersionedItemDeleted(workflow.path),
        ItemDestroyed(lock.path))).orThrow
      .checkConsistencyForDeletedItems(Seq(lock.path))
      .orThrow

    // Destroy AgentRef
    assert(myControllerState
      .applyEvent(ItemDestroyed(fileWatch.path)).orThrow
      .checkConsistencyForDeletedItems(Seq(agentRef.path))
      == Left(ItemIsStillReferencedProblem(agentRef.path, workflow.id)))
    assert(myControllerState.checkConsistencyForDeletedItems(Seq(agentRef.path)) ==
      Left(Problem.Combined(Set(
        ItemIsStillReferencedProblem(agentRef.path, fileWatch.path),
        ItemIsStillReferencedProblem(agentRef.path, workflow.id)))))
    assert(myControllerState
      .applyEvents(Seq(
        ItemDestroyed(fileWatch.path),
        VersionAdded(VersionId("DELETE")),
        VersionedItemDeleted(workflow.path))).orThrow
      .checkConsistencyForDeletedItems(Seq(agentRef.path))
      == Right(()))

    // Destroy JobResource
    assert(myControllerState.checkConsistencyForDeletedItems(Seq(jobResource.path)) ==
      Left(ItemIsStillReferencedProblem(jobResource.path, workflow.id)))
    assert(myControllerState
      .applyEvent(VersionAdded(VersionId("DELETE"))).orThrow
      .applyEvent(VersionedItemDeleted(workflow.path)).orThrow
      .checkConsistencyForDeletedItems(Seq(jobResource.path)) == Right(()))

    // Destroy FileWatch
    assert(myControllerState.checkConsistencyForDeletedItems(Seq(fileWatch.path)) == Right(()))

    // Delete Workflow
    assert(myControllerState.checkConsistencyForDeletedItems(Seq(workflow.id)) ==
      Left(ItemIsStillReferencedProblem(workflow.id, fileWatch.path)))
    controllerState  // with order
      .applyEvent(ItemDestroyed(fileWatch.path)).orThrow
      .checkConsistencyForDeletedItems(Seq(workflow.id))
      .orThrow
    myControllerState // without order
      .applyEvent(ItemDestroyed(fileWatch.path)).orThrow
      .checkConsistencyForDeletedItems(Seq(workflow.id))
      .orThrow
    succeed
  }

  "fromIterator is the reverse of toSnapshotObservable + EventId" in {
    controllerState.toSnapshotObservable.toListL
      .flatMap(snapshotObjects =>
        ControllerState.fromObservable(Observable.fromIterable(snapshotObjects)))
      .map(expectedState => assertEqual(controllerState, expectedState))
      .runToFuture
  }

  private val expectedSnapshotJsonArray = json"""[
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
              "A": "https://A",
              "B": "https://B"
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
        "TYPE": "AgentRefState",
        "agentRef": {
          "path": "AGENT",
          "uri": "https://AGENT",
          "itemRevision": 0
        },
        "couplingState": {
          "TYPE": "Reset"
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
        "TYPE": "UnsignedSimpleItemAdded",
        "item": {
          "TYPE": "FileWatch",
          "path": "WATCH",
          "workflowPath": "WORKFLOW",
          "agentPath": "AGENT",
          "directory": "/tmp/directory",
          "delay": 0,
          "itemRevision": 7
        }
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
          "string": "{\"TYPE\":\"JobResource\",\"path\":\"JOB-RESOURCE\",\"settings\":{},\"env\":{}}",
          "signature": {
            "TYPE": "Silly",
            "signatureString": "SILLY-SIGNATURE"
          }
        }
      }, {
        "TYPE": "VersionAdded",
        "versionId": "1.0"
      }, {
        "TYPE": "VersionedItemAdded",
        "signed": {
          "signature": {
            "TYPE": "Silly",
            "signatureString": "SILLY-SIGNATURE"
          },
          "string": "{\"TYPE\":\"Workflow\",\"path\":\"WORKFLOW\",\"versionId\":\"1.0\",\"instructions\":[{\"TYPE\":\"Lock\",\"lockPath\":\"LOCK\",\"lockedWorkflow\":{\"instructions\":[{\"TYPE\":\"Execute.Anonymous\",\"job\":{\"agentPath\":\"AGENT\",\"executable\":{\"TYPE\":\"ScriptExecutable\",\"script\":\"\"},\"jobResourcePaths\":[\"JOB-RESOURCE\"],\"taskLimit\":1}}]}}]}"
        }
      }, {
        "TYPE": "ItemAttachable",
        "agentPath": "AGENT",
        "key": "JobResource:JOB-RESOURCE"
      },
      {
        "TYPE": "ItemDestructionMarked",
        "key": "OrderWatch:WATCH"
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
            "path": "WORKFLOW",
            "versionId": "1.0"
          }
        }
      }
    ]"""

  "toSnapshotObservable JSON" in {
    implicit val x = ControllerState.snapshotObjectJsonCodec
    for {
      jsonArray <- controllerState.toSnapshotObservable.toListL.runToFuture
      assertion <- testJson(jsonArray, expectedSnapshotJsonArray)
    } yield assertion
  }

  "ControllerStateBuilder.addSnapshotObject" in {
    ControllerState.snapshotObjectJsonCodec
    val builder = new ControllerStateBuilder
    expectedSnapshotJsonArray.asArray.get
      .map(json => ControllerState.snapshotObjectJsonCodec.decodeJson(json).toChecked.orThrow)
      .foreach(builder.addSnapshotObject)
    builder.onAllSnapshotsAdded()
    assert(builder.result() == controllerState)
  }

  "keyToItem" in {
    assert(!controllerState.keyToItem.contains(WorkflowPath("UNKNOWN") ~ "1"))

    assert(controllerState.keyToItem.get(jobResource.path) == Some(jobResource))
    assert(controllerState.keyToItem.get(agentRef.path) == Some(agentRef))
    assert(controllerState.keyToItem.get(lock.path) == Some(lock))
    assert(controllerState.keyToItem.get(fileWatch.path) == Some(fileWatch))
    assert(controllerState.keyToItem.get(workflow.id) == Some(workflow))

    assert(controllerState.keyToItem.keySet == Set(
      jobResource.path, agentRef.path, lock.path, fileWatch.path, workflow.id))
  }
}

object ControllerStateTest
{
  private lazy val jobResource = JobResource(JobResourcePath("JOB-RESOURCE"))
  private lazy val itemSigner = new ItemSigner(SillySigner.Default, ControllerState.signableItemJsonCodec)
  private lazy val signedJobResource = itemSigner.sign(jobResource)
  private val agentRef = AgentRef(AgentPath("AGENT"), Uri("https://AGENT"), Some(ItemRevision(0)))
  private val lock = Lock(LockPath("LOCK"), itemRevision = Some(ItemRevision(7)))
  private val versionId = VersionId("1.0")
  private val workflow = Workflow(WorkflowPath("WORKFLOW") ~ versionId, Seq(
    LockInstruction(lock.path, None, Workflow.of(
      Execute(WorkflowJob(agentRef.path, ScriptExecutable(""), jobResourcePaths = Seq(jobResource.path)))))))
  private val signedWorkflow = itemSigner.sign(workflow)
  private val orderId = OrderId("ORDER")
  private val fileWatch = FileWatch(
    OrderWatchPath("WATCH"),
    workflow.path,
    AgentPath("AGENT"),
    "/tmp/directory",
    itemRevision = Some(ItemRevision(7)))
}
