package js7.tests

import com.google.common.io.MoreFiles.touch
import java.nio.file.Files.delete
import js7.base.auth.Admission
import js7.base.configutils.Configs._
import js7.base.io.file.FileUtils.withTemporaryFile
import js7.base.problem.Checked.Ops
import js7.base.problem.Problem
import js7.base.thread.MonixBlocking.syntax._
import js7.base.time.ScalaTime._
import js7.controller.client.AkkaHttpControllerApi.admissionsToApiResources
import js7.data.agent.AgentId
import js7.data.event.EventSeq
import js7.data.item.ItemOperation.{AddVersion, SimpleDelete}
import js7.data.item.VersionId
import js7.data.lock.Acquired.Available
import js7.data.lock.{Lock, LockId, LockState}
import js7.data.order.OrderEvent._
import js7.data.order.{FreshOrder, OrderEvent, OrderId, Outcome}
import js7.data.value.ValuePrinter.quoteString
import js7.data.workflow.position.Position
import js7.data.workflow.{Workflow, WorkflowParser, WorkflowPath}
import js7.proxy.ControllerApi
import js7.tests.LockTest._
import js7.tests.testenv.ControllerAgentForScalaTest
import js7.tests.testenv.DirectoryProvider.{script, waitingForFileScript}
import monix.execution.Scheduler.Implicits.global
import monix.reactive.Observable
import org.scalatest.freespec.AnyFreeSpec
import scala.collection.immutable.Queue
import scala.util.Random

final class LockTest extends AnyFreeSpec with ControllerAgentForScalaTest
{
  protected val agentIds = Seq(agentId, bAgentId)
  protected val versionedItems = Nil
  override protected def controllerConfig = config"""
    js7.web.server.auth.loopback-is-public = on
    js7.controller.agent-driver.command-batch-delay = 0ms
    js7.controller.agent-driver.event-buffer-delay = 5ms
    """
  override protected def agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = on
    """
  private lazy val versionIdIterator = Iterator.from(1).map(i => VersionId(i.toString))
  private lazy val controllerApi = new ControllerApi(
    admissionsToApiResources(Seq(Admission(controller.localUri, None)))(controller.actorSystem))

  override def beforeAll() = {
    super.beforeAll()
    val items = Seq(
      Lock(lockId, limit = 1),
      Lock(lock2Id, limit = 1),
      Lock(limit2LockId, limit = 2))
    controllerApi.updateSimpleItems(items).await(99.s).orThrow
  }


  "Run some orders at differant agents with a lock with limit=1" in {
    withTemporaryFile("LockTest-", ".tmp") { file =>
      val workflow = defineWorkflow(s"""
        define workflow {
          lock (lock = "LOCK") {
            job JOB-1;
            job JOB-2;
          };
          define job JOB-1 {
            execute agent="${agentId.string}", script=${quoteString(waitingForFileScript(file))}, taskLimit = 100;
          }
          define job JOB-2 {
            execute agent="${bAgentId.string}", script=${quoteString(waitingForFileScript(file))}, taskLimit = 100;
          }
        }""")

      delete(file)
      val a = OrderId("🔵")
      controller.addOrder(FreshOrder(a, workflow.path)).await(99.s).orThrow
      assert(controller.eventWatch.await[OrderLockAcquired](_.key == a).map(_.value).nonEmpty)

      val queuedOrderIds = for (i <- 1 to 10) yield OrderId(s"🟠-$i")
      for (orderId <- queuedOrderIds) {
        controller.addOrder(FreshOrder(orderId, workflow.path)).await(99.s).orThrow
        assert(controller.eventWatch.await[OrderLockQueued](_.key == orderId).map(_.value).nonEmpty)
      }

      touch(file)

      assert(controller.eventWatch.await[OrderTerminated](_.key == a).map(_.value) == Seq(a <-: OrderFinished))
      assert(controller.eventWatch.keyedEvents[OrderEvent](a) == Seq(
        OrderAdded(workflow.id),
        OrderStarted,
        OrderLockAcquired(lockId, None),
          OrderAttachable(agentId),
          OrderAttached(agentId),
          OrderProcessingStarted,
          OrderProcessed(Outcome.succeededRC0),
          OrderMoved(Position(0) / "lock" % 1),
          OrderDetachable,
          OrderDetached,

          OrderAttachable(bAgentId),
          OrderAttached(bAgentId),
          OrderProcessingStarted,
          OrderProcessed(Outcome.succeededRC0),
          OrderMoved(Position(0) / "lock" % 2),
          OrderDetachable,
          OrderDetached,
        OrderLockReleased(lockId),
        OrderFinished))

      for (orderId <- queuedOrderIds) {
        assert(controller.eventWatch.await[OrderTerminated](_.key == orderId).map(_.value) == Seq(orderId <-: OrderFinished))
        assert(controller.eventWatch.keyedEvents[OrderEvent](orderId) == Seq(
          OrderAdded(workflow.id),
          OrderStarted,
          OrderLockQueued(lockId, None),
          OrderLockAcquired(lockId, None),
            OrderAttachable(agentId),
            OrderAttached(agentId),
            OrderProcessingStarted,
            OrderProcessed(Outcome.succeededRC0),
            OrderMoved(Position(0) / "lock" % 1),
            OrderDetachable,
            OrderDetached,

            OrderAttachable(bAgentId),
            OrderAttached(bAgentId),
            OrderProcessingStarted,
            OrderProcessed(Outcome.succeededRC0),
            OrderMoved(Position(0) / "lock" % 2),
            OrderDetachable,
            OrderDetached,
          OrderLockReleased(lockId),
          OrderFinished))
      }

      for (pair <- queuedOrderIds.sliding(2).toSeq) {
        assert(controller.eventWatch.await[OrderLockAcquired](_.key == pair(0)).map(_.eventId).head <
               controller.eventWatch.await[OrderLockAcquired](_.key == pair(1)).map(_.eventId).head)
      }
      assert(controller.controllerState.await(99.s).idToLockState(lockId) ==
        LockState(Lock(lockId, limit = 1), Available, Queue.empty))
    }
  }

  "After releasing a lock of 2, two orders with count=1 each start simultaneously" in {
    val workflow1 = defineWorkflow(WorkflowPath("WORKFLOW-1"), s"""
      define workflow {
        lock (lock="${limit2LockId.string}", count=1) {
          execute agent="AGENT", script="${script(50.ms)}", taskLimit=99
        }
      }""")
    val workflow2 = defineWorkflow(WorkflowPath("WORKFLOW-2"), s"""
      define workflow {
        lock (lock="${limit2LockId.string}", count=2) {
          execute agent="AGENT", script="${script(100.ms)}", taskLimit=99
        }
      }""")
    val order2Id = OrderId("🟥-TWO")
    val aOrderId = OrderId("🟥-A")
    val bOrderId = OrderId("🟥-B")
    controller.addOrder(FreshOrder(order2Id, workflow2.path)).await(99.s).orThrow
    controller.eventWatch.await[OrderLockAcquired](_.key == order2Id)
    controller.addOrder(FreshOrder(aOrderId, workflow1.path)).await(99.s).orThrow
    controller.addOrder(FreshOrder(bOrderId, workflow1.path)).await(99.s).orThrow
    controller.eventWatch.await[OrderTerminated](_.key == aOrderId)
    controller.eventWatch.await[OrderTerminated](_.key == bOrderId)
    val EventSeq.NonEmpty(stampedEvents) = controller.eventWatch.all[OrderLockEvent]
    val aAquired = stampedEvents.find(stamped => stamped.value.key == aOrderId && stamped.value.event.isInstanceOf[OrderLockAcquired]).get
    val bAquired = stampedEvents.find(stamped => stamped.value.key == bOrderId && stamped.value.event.isInstanceOf[OrderLockAcquired]).get
    val aReleased = stampedEvents.find(stamped => stamped.value.key == aOrderId && stamped.value.event.isInstanceOf[OrderLockReleased]).get
    val bReleased = stampedEvents.find(stamped => stamped.value.key == aOrderId && stamped.value.event.isInstanceOf[OrderLockReleased]).get
    assert(aAquired.eventId < bReleased.eventId, "- a acquired lock after b released")
    assert(bAquired.eventId < aReleased.eventId, "- b acquired lock after a released")
    assert(controller.controllerState.await(99.s).idToLockState(limit2LockId) ==
      LockState(Lock(limit2LockId, limit = 2), Available, Queue.empty))
  }

  "Multiple orders with count=1 and count=2 finish" in {
    val workflow1 = defineWorkflow(WorkflowPath("WORKFLOW-1"), s"""
      define workflow {
        lock (lock="${limit2LockId.string}", count=1) {
          execute agent="AGENT", script="${script(10.ms)}", taskLimit=99
        }
      }""")
    val workflow2 = defineWorkflow(WorkflowPath("WORKFLOW-2"), s"""
      define workflow {
        lock (lock="${limit2LockId.string}", count=2) {
          execute agent="AGENT", script="${script(10.ms)}", taskLimit=99
        }
      }""")
    val orders = Random.shuffle(
      for (workflow <- Seq(workflow1, workflow2); i <- 1 to 100) yield
        FreshOrder(OrderId(s"${workflow.path.string}-$i"), workflow.path))
    controllerApi.addOrders(Observable.from(orders)).await(99.s).orThrow
    val terminated = for (order <- orders) yield controller.eventWatch.await[OrderTerminated](_.key == order.id)
    for (keyedEvent <- terminated.map(_.last.value))  assert(keyedEvent.event == OrderFinished, s"- ${keyedEvent.key}")
    assert(controller.controllerState.await(99.s).idToLockState(limit2LockId) ==
      LockState(Lock(limit2LockId, limit = 2), Available, Queue.empty))
  }

  "Failed order" in {
    val workflow = defineWorkflow(workflowNotation = """
      define workflow {
        lock (lock = "LOCK") {
          lock (lock = "LOCK-2") {
            fail;
          }
        }
      }""")

    val orderId = OrderId("🟦")
    controller.addOrder(FreshOrder(orderId, workflow.path)).await(99.s).orThrow
    controller.eventWatch.await[OrderTerminated](_.key == orderId)
    assert(controller.eventWatch.keyedEvents[OrderEvent](orderId) == Seq(
      OrderAdded(workflow.id),
      OrderStarted,
      OrderLockAcquired(lockId, None),
      OrderLockAcquired(lock2Id, None),
      OrderFailed(Position(0), Some(Outcome.failed), lockIds = Seq(lock2Id, lockId))))
    assert(controller.controllerState.await(99.s).idToLockState(lockId) ==
      LockState(Lock(lockId, limit = 1), Available, Queue.empty))
  }

  "Failed order in try/catch" in {
    val workflow = defineWorkflow(workflowNotation = """
      define workflow {
        lock (lock = "LOCK") {
          try {
            lock (lock = "LOCK-2") fail;
          } catch {
            execute agent="AGENT", script=":";
          }
        }
      }""")

    val orderId = OrderId("🟨")
    controller.addOrder(FreshOrder(orderId, workflow.path)).await(99.s).orThrow
    controller.eventWatch.await[OrderTerminated](_.key == orderId)
    assert(controller.eventWatch.keyedEvents[OrderEvent](orderId) == Seq(
      OrderAdded(workflow.id),
      OrderStarted,
      OrderLockAcquired(lockId, None),
      OrderMoved(Position(0) / "lock" % 0 / "try+0" % 0),
      OrderLockAcquired(lock2Id, None),
      OrderCatched(Position(0) / "lock" % 0 / "catch+0" % 0, Some(Outcome.failed), lockIds = Seq(lock2Id)),
      OrderAttachable(agentId),
      OrderAttached(agentId),
      OrderProcessingStarted,
      OrderProcessed(Outcome.succeededRC0),
      OrderMoved(Position(0) / "lock" % 1),
      OrderDetachable,
      OrderDetached,
      OrderLockReleased(lockId),
      OrderFinished))
    assert(controller.controllerState.await(99.s).idToLockState(lockId) ==
      LockState(Lock(lockId, limit = 1), Available, Queue.empty))
  }

  "Failed forked order" in {
    val workflow = defineWorkflow(workflowNotation = """
      define workflow {
        fork {
          "BRANCH": {
            lock (lock = "LOCK") {
              fail;
            }
          }
        }
      }""")
    val orderId = OrderId("🟩")
    controller.addOrder(FreshOrder(orderId, workflow.path)).await(99.s).orThrow

    controller.eventWatch.await[OrderTerminated](_.key == orderId)
    assert(controller.eventWatch.keyedEvents[OrderEvent](orderId) == Seq(
      OrderAdded(workflow.id),
      OrderStarted,
      OrderForked(Seq(OrderForked.Child("BRANCH", orderId | "BRANCH"))),
      OrderJoined(Outcome.failed),
      OrderFailed(Position(0))))

    assert(controller.eventWatch.keyedEvents[OrderEvent](orderId | "BRANCH") == Seq(
      OrderLockAcquired(lockId, None),
      OrderFailedInFork(Position(0) / "fork+BRANCH" % 0, Some(Outcome.failed), lockIds = Seq(lockId))))

    assert(controller.controllerState.await(99.s).idToLockState(lockId) ==
      LockState(Lock(lockId, limit = 1), Available, Queue.empty))
  }

  "Forked order with same lock" in {
    val workflow = defineWorkflow(workflowNotation = """
      define workflow {
        lock (lock = "LOCK") {
          fork {
            "BRANCH": {
              lock (lock = "LOCK") {}
            }
          }
        }
      }""")
    val orderId = OrderId("🟪")
    controller.addOrder(FreshOrder(orderId, workflow.path)).await(99.s).orThrow

    controller.eventWatch.await[OrderTerminated](_.key == orderId)
    assert(controller.eventWatch.keyedEvents[OrderEvent](orderId) == Seq(
      OrderAdded(workflow.id),
      OrderStarted,
      OrderLockAcquired(lockId, None),
      OrderForked(Seq(OrderForked.Child("BRANCH", orderId | "BRANCH"))),
      OrderJoined(Outcome.failed),
      OrderFailed(Position(0), lockIds = Seq(lockId))))

    assert(controller.eventWatch.keyedEvents[OrderEvent](orderId | "BRANCH") == Seq(
      OrderFailedInFork(Position(0) / "lock" % 0 / "fork+BRANCH" % 0, Some(Outcome.Disrupted(Problem(
        "Lock:LOCK has already been acquired by parent Order:🟪"))))))

    assert(controller.controllerState.await(99.s).idToLockState(lockId) ==
      LockState(Lock(lockId, limit = 1), Available, Queue.empty))
  }

  "Acquire too much" in {
    val workflow = defineWorkflow(workflowNotation = """
      define workflow {
        lock (lock = "LOCK", count = 2) {}
      }""")
    val orderId = OrderId("⬛️")
    controller.addOrder(FreshOrder(orderId, workflow.path)).await(99.s).orThrow

    controller.eventWatch.await[OrderTerminated](_.key == orderId)
    assert(controller.eventWatch.keyedEvents[OrderEvent](orderId) == Seq(
      OrderAdded(workflow.id),
      OrderStarted,
      OrderFailed(Position(0), Some(Outcome.Disrupted(Problem("Cannot fulfill lock count=2 with Lock:LOCK limit=1"))))))

    assert(controller.controllerState.await(99.s).idToLockState(lockId) ==
      LockState(Lock(lockId, limit = 1), Available, Queue.empty))
  }

  "Lock deletion is still not supported" in {
    val v = VersionId("DELETE")
    assert(controllerApi.updateItems(Observable(
      AddVersion(v),
      SimpleDelete(lockId)
    )).await(99.s) == Left(Problem("Event 'SimpleItemDeleted(Lock:LOCK)' cannot be applied: Locks are not deletable (in this version)")))
  }

  private def defineWorkflow(workflowNotation: String): Workflow =
    defineWorkflow(WorkflowPath("WORKFLOW"), workflowNotation)

  private def defineWorkflow(workflowPath: WorkflowPath, workflowNotation: String): Workflow = {
    val versionId = versionIdIterator.next()
    val workflow = WorkflowParser.parse(workflowPath ~ versionId, workflowNotation).orThrow
    directoryProvider.updateVersionedItems(controller, versionId, Seq(workflow))
    workflow
  }
}

object LockTest {
  private val agentId = AgentId("AGENT")
  private val bAgentId = AgentId("B-AGENT")
  private val lockId = LockId("LOCK")
  private val lock2Id = LockId("LOCK-2")
  private val limit2LockId = LockId("LOCK-LIMIT-2")
}
