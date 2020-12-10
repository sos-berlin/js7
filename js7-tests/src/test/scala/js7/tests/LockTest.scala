package js7.tests

import com.google.common.io.MoreFiles.touch
import java.nio.file.Files.delete
import js7.base.auth.Admission
import js7.base.problem.Checked.Ops
import js7.base.problem.Problem
import js7.base.time.ScalaTime._
import js7.common.configutils.Configs._
import js7.common.scalautil.FileUtils.withTemporaryFile
import js7.common.scalautil.MonixUtils.syntax._
import js7.controller.client.AkkaHttpControllerApi.admissionsToApiResources
import js7.data.agent.AgentName
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
import js7.tests.testenv.DirectoryProvider.waitingForFileScript
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AnyFreeSpec
import scala.collection.immutable.Queue

final class LockTest extends AnyFreeSpec with ControllerAgentForScalaTest
{
  protected val agentNames = Seq(agentName)
  protected val inventoryItems = Nil
  override protected def controllerConfig = config"""
    js7.web.server.auth.public = on
    """
  override protected def agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = on
    """
  private lazy val versionIdIterator = Iterator.from(1).map(i => VersionId(i.toString))
  private lazy val controllerApi = new ControllerApi(
    admissionsToApiResources(Seq(Admission(controller.localUri, None)))(controller.actorSystem))

  override def beforeAll() = {
    super.beforeAll()
    controllerApi.updateLocks(Seq(Lock(lockId), Lock(lock2Name))).await(99.s).orThrow
  }

  "TEST" in {
    withTemporaryFile("LockTest-", ".tmp") { file =>
      val workflow = defineWorkflow(s"""
        define workflow {
          lock (lock = "LOCK") {
            execute agent="AGENT", script=${quoteString(waitingForFileScript(file))}, taskLimit = 100;
          }
        }""")

      delete(file)
      val a = OrderId("🔵")
      controller.addOrder(FreshOrder(a, workflow.path)).await(99.s).orThrow
      assert(controller.eventWatch.await[OrderLockAcquired](_.key == a).map(_.value).nonEmpty)

      val queuedOrderIds = for (i <- 1 to 10) yield OrderId(s"🟠-${i}")
      for (orderId <- queuedOrderIds) {
        controller.addOrder(FreshOrder(orderId, workflow.path)).await(99.s).orThrow
        assert(controller.eventWatch.await[OrderLockQueued](_.key == orderId).map(_.value).nonEmpty)
      }

      touch(file)

      assert(controller.eventWatch.await[OrderTerminated](_.key == a).map(_.value) == Seq(a <-: OrderFinished))
      assert(controller.eventWatch.keyedEvents[OrderEvent](a) == Seq(
        OrderAdded(workflow.id),
        OrderStarted,
        OrderLockAcquired(lockId, exclusively = true),
        OrderAttachable(agentName),
        OrderAttached(agentName),
        OrderProcessingStarted,
        OrderProcessed(Outcome.succeededRC0),
        OrderMoved(Position(0) / "lock" % 1),
        OrderDetachable,
        OrderDetached,
        OrderLockReleased(lockId),
        OrderFinished))

      for (orderId <- queuedOrderIds) {
        assert(controller.eventWatch.await[OrderTerminated](_.key == orderId).map(_.value) == Seq(orderId <-: OrderFinished))
        assert(controller.eventWatch.keyedEvents[OrderEvent](orderId) == Seq(
          OrderAdded(workflow.id),
          OrderStarted,
          OrderLockQueued(lockId),
          OrderLockAcquired(lockId, exclusively = true),
          OrderAttachable(agentName),
          OrderAttached(agentName),
          OrderProcessingStarted,
          OrderProcessed(Outcome.succeededRC0),
          OrderMoved(Position(0) / "lock" % 1),
          OrderDetachable,
          OrderDetached,
          OrderLockReleased(lockId),
          OrderFinished))
      }

      for (pair <- queuedOrderIds.sliding(2).toSeq) {
        assert(controller.eventWatch.await[OrderLockAcquired](_.key == pair(0)).map(_.eventId).head <
               controller.eventWatch.await[OrderLockAcquired](_.key == pair(1)).map(_.eventId).head)
      }
      assert(controller.controllerState.await(99.s).nameToLockState(lockId) ==
        LockState(Lock(lockId), Available, Queue.empty))
    }
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
      OrderLockAcquired(lockId, exclusively = true),
      OrderLockAcquired(lock2Name, exclusively = true),
      OrderFailed(Position(0), Some(Outcome.failed), lockIds = Seq(lock2Name, lockId))))
    assert(controller.controllerState.await(99.s).nameToLockState(lockId) ==
      LockState(Lock(lockId), Available, Queue.empty))
  }

  "Failed order in try/catch" in {
    val workflow = defineWorkflow(workflowNotation = """
      define workflow {
        lock (lock = "LOCK") {
          try {
            lock (lock = "LOCK-2") {
              fail;
            }
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
      OrderLockAcquired(lockId, exclusively = true),
      OrderMoved(Position(0) / "lock" % 0 / "try+0" % 0),
      OrderLockAcquired(lock2Name, exclusively = true),
      OrderCatched(Position(0) / "lock" % 0 / "catch+0" % 0, Some(Outcome.failed), lockIds = Seq(lock2Name)),
      OrderAttachable(agentName),
      OrderAttached(agentName),
      OrderProcessingStarted,
      OrderProcessed(Outcome.succeededRC0),
      OrderMoved(Position(0) / "lock" % 1),
      OrderDetachable,
      OrderDetached,
      OrderLockReleased(lockId),
      OrderFinished))
    assert(controller.controllerState.await(99.s).nameToLockState(lockId) ==
      LockState(Lock(lockId), Available, Queue.empty))
  }

  "Failed forked order" in {
    withTemporaryFile("LockTest-", ".tmp") { file =>
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
        OrderLockAcquired(lockId, exclusively = true),
        OrderFailedInFork(Position(0) / "fork+BRANCH" % 0, Some(Outcome.failed), lockIds = Seq(lockId))))

      assert(controller.controllerState.await(99.s).nameToLockState(lockId) ==
        LockState(Lock(lockId), Available, Queue.empty))
    }
  }

  "Forked order with same lock" in {
    withTemporaryFile("LockTest-", ".tmp") { file =>
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
        OrderLockAcquired(lockId, exclusively = true),
        OrderForked(Seq(OrderForked.Child("BRANCH", orderId | "BRANCH"))),
        OrderJoined(Outcome.failed),
        OrderFailed(Position(0), lockIds = Seq(lockId))))

      assert(controller.eventWatch.keyedEvents[OrderEvent](orderId | "BRANCH") == Seq(
        OrderFailedInFork(Position(0) / "lock" % 0 / "fork+BRANCH" % 0, Some(Outcome.Disrupted(Problem(
          "Lock:LOCK has already been acquired by parent Order:🟪"))))))

      assert(controller.controllerState.await(99.s).nameToLockState(lockId) ==
        LockState(Lock(lockId), Available, Queue.empty))
    }
  }

  private def defineWorkflow(workflowNotation: String): Workflow = {
    val workflowPath = WorkflowPath("/WORKFLOW")
    val versionId = versionIdIterator.next()
    val workflow = WorkflowParser.parse(workflowPath ~ versionId, workflowNotation).orThrow
    directoryProvider.updateRepo(controller, versionId, Seq(workflow))
    workflow
  }
}

object LockTest {
  private val agentName = AgentName("AGENT")
  private val lockId = LockId("LOCK")
  private val lock2Name = LockId("LOCK-2")
}
