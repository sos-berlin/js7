package js7.tests

import java.nio.file.Files.delete
import js7.base.configutils.Configs.*
import js7.base.io.file.FileUtils.{touchFile, withTemporaryFile}
import js7.base.problem.Checked.Ops
import js7.base.problem.Problem
import js7.base.test.Test
import js7.base.thread.MonixBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.data.Problems.ItemIsStillReferencedProblem
import js7.data.agent.AgentPath
import js7.data.controller.ControllerCommand.{AnswerOrderPrompt, CancelOrders, DeleteOrdersWhenTerminated}
import js7.data.item.BasicItemEvent.{ItemDeleted, ItemDetached}
import js7.data.item.ItemAttachedState.Attached
import js7.data.item.ItemOperation.{AddVersion, DeleteSimple, RemoveVersioned}
import js7.data.item.VersionedEvent.VersionAdded
import js7.data.item.{ItemRevision, Repo, VersionId}
import js7.data.lock.Acquired.Available
import js7.data.lock.{Lock, LockPath, LockState}
import js7.data.order.OrderEvent.*
import js7.data.order.{FreshOrder, OrderEvent, OrderId, Outcome}
import js7.data.subagent.SubagentId
import js7.data.value.StringValue
import js7.data.value.ValuePrinter.quoteString
import js7.data.value.expression.Expression.StringConstant
import js7.data.value.expression.ExpressionParser.expr
import js7.data.workflow.instructions.{Fail, Finish, Fork, LockInstruction, Prompt}
import js7.data.workflow.position.{BranchId, Position}
import js7.data.workflow.{Workflow, WorkflowId, WorkflowParser, WorkflowPath}
import js7.tests.LockTest.*
import js7.tests.jobs.{EmptyJob, SleepJob}
import js7.tests.testenv.{BlockingItemUpdater, ControllerAgentForScalaTest}
import js7.tests.testenv.DirectoryProvider.{script, toLocalSubagentId, waitingForFileScript}
import monix.execution.Scheduler.Implicits.traced
import monix.reactive.Observable
import scala.collection.immutable.Queue
import scala.util.Random

final class LockTest extends Test with ControllerAgentForScalaTest with BlockingItemUpdater
{
  protected val agentPaths = Seq(agentPath, bAgentPath)
  protected val items = Seq(
    Lock(lockPath),
    Lock(lock2Path),
    Lock(limit2LockPath, limit = 2),
    promptInLockWorkflow)
  override protected def controllerConfig = config"""
    js7.auth.users.TEST-USER.permissions = [ UpdateItem ]
    js7.controller.agent-driver.command-batch-delay = 0ms
    js7.controller.agent-driver.event-buffer-delay = 0ms
    """
  override protected def agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = on
    """

  "Run some orders at different agents with a lock with limit=1" in {
    withTemporaryFile("LockTest-", ".tmp") { file =>
      val workflow = defineWorkflow(s"""
        define workflow {
          lock (lock = "LOCK") {
            job `JOB-1`;
            job `JOB-2`;
          };
          define job `JOB-1` {
            execute agent="${agentPath.string}", script=${quoteString(waitingForFileScript(file))}, parallelism = 100;
          }
          define job `JOB-2` {
            execute agent="${bAgentPath.string}", script=${quoteString(waitingForFileScript(file))}, parallelism = 100;
          }
        }""")
      assert(workflow.referencedItemPaths.toSet == Set(lockPath, agentPath, bAgentPath))

      delete(file)
      val a = OrderId("🔵")
      controllerApi.addOrder(FreshOrder(a, workflow.path, deleteWhenTerminated = true))
        .await(99.s).orThrow
      assert(controller.eventWatch.await[OrderLockAcquired](_.key == a).map(_.value).nonEmpty)

      val queuedOrderIds = for (i <- 1 to 10) yield OrderId(s"🟠-$i")
      for (orderId <- queuedOrderIds) {
        controllerApi.addOrder(FreshOrder(orderId, workflow.path, deleteWhenTerminated = true))
          .await(99.s).orThrow
        assert(controller.eventWatch.await[OrderLockQueued](_.key == orderId).map(_.value).nonEmpty)
      }

      touchFile(file)

      assert(controller.eventWatch.await[OrderTerminated](_.key == a).map(_.value) == Seq(a <-: OrderFinished))
      controller.eventWatch.await[OrderDeleted](_.key == a)
      assert(controller.eventWatch.eventsByKey[OrderEvent](a) == Seq(
        OrderAdded(workflow.id, deleteWhenTerminated = true),
        OrderStarted,
        OrderLockAcquired(lockPath, None),
          OrderAttachable(agentPath),
          OrderAttached(agentPath),
          OrderProcessingStarted(subagentId),
          OrderProcessed(Outcome.succeededRC0),
          OrderMoved(Position(0) / "lock" % 1),
          OrderDetachable,
          OrderDetached,

          OrderAttachable(bAgentPath),
          OrderAttached(bAgentPath),
          OrderProcessingStarted(bSubagentId),
          OrderProcessed(Outcome.succeededRC0),
          OrderMoved(Position(0) / "lock" % 2),
          OrderDetachable,
          OrderDetached,
        OrderLockReleased(lockPath),
        OrderFinished,
        OrderDeleted))

      for (orderId <- queuedOrderIds) {
        assert(controller.eventWatch.await[OrderTerminated](_.key == orderId).map(_.value) == Seq(orderId <-: OrderFinished))
        controller.eventWatch.await[OrderDeleted](_.key == orderId)
        assert(controller.eventWatch.eventsByKey[OrderEvent](orderId) == Seq(
          OrderAdded(workflow.id, deleteWhenTerminated = true),
          OrderStarted,
          OrderLockQueued(lockPath, None),
          OrderLockAcquired(lockPath, None),
            OrderAttachable(agentPath),
            OrderAttached(agentPath),
            OrderProcessingStarted(subagentId),
            OrderProcessed(Outcome.succeededRC0),
            OrderMoved(Position(0) / "lock" % 1),
            OrderDetachable,
            OrderDetached,

            OrderAttachable(bAgentPath),
            OrderAttached(bAgentPath),
            OrderProcessingStarted(bSubagentId),
            OrderProcessed(Outcome.succeededRC0),
            OrderMoved(Position(0) / "lock" % 2),
            OrderDetachable,
            OrderDetached,
          OrderLockReleased(lockPath),
          OrderFinished,
          OrderDeleted))
      }

      for (pair <- queuedOrderIds.sliding(2).toSeq) {
        assert(controller.eventWatch.await[OrderLockAcquired](_.key == pair(0)).map(_.eventId).head <
               controller.eventWatch.await[OrderLockAcquired](_.key == pair(1)).map(_.eventId).head)
      }
      assert(controllerState.keyTo(LockState)(lockPath) ==
        LockState(
          Lock(lockPath, itemRevision = Some(ItemRevision(0))),
          Available,
          Queue.empty))
    }
  }

  "After releasing a lock of 2, two orders with count=1 each start simultaneously" in {
    val workflow1 = updateItem(Workflow(
      WorkflowPath("FINISH-IN-FORK"),
      Seq(
        LockInstruction(
          limit2LockPath,
          count = Some(1),
          lockedWorkflow = Workflow.of(
            SleepJob.execute(agentPath, parallelism = 99, arguments = Map(
              "sleep" -> expr("0.050"))))))))

    val workflow2 = updateItem(Workflow(
      WorkflowPath("WORKFLOW-2"),
      Seq(
        LockInstruction(
          limit2LockPath,
          count = Some(2),
          lockedWorkflow = Workflow.of(
            SleepJob.execute(agentPath, parallelism = 99, arguments = Map(
              "sleep" -> expr("0.100"))))))))

    val order2Id = OrderId("🟥-TWO")
    val aOrderId = OrderId("🟥-A")
    val bOrderId = OrderId("🟥-B")
    controllerApi.addOrder(FreshOrder(order2Id, workflow2.path, deleteWhenTerminated = true))
      .await(99.s).orThrow
    controller.eventWatch.await[OrderLockAcquired](_.key == order2Id)
    controllerApi.addOrder(FreshOrder(aOrderId, workflow1.path, deleteWhenTerminated = true))
      .await(99.s).orThrow
    controllerApi.addOrder(FreshOrder(bOrderId, workflow1.path, deleteWhenTerminated = true))
      .await(99.s).orThrow
    controller.eventWatch.await[OrderTerminated](_.key == aOrderId)
    controller.eventWatch.await[OrderTerminated](_.key == bOrderId)
    val stampedEvents = controller.eventWatch.allStamped[OrderLockEvent]
    val aAquired = stampedEvents.find(stamped => stamped.value.key == aOrderId && stamped.value.event.isInstanceOf[OrderLockAcquired]).get
    val bAquired = stampedEvents.find(stamped => stamped.value.key == bOrderId && stamped.value.event.isInstanceOf[OrderLockAcquired]).get
    val aReleased = stampedEvents.find(stamped => stamped.value.key == aOrderId && stamped.value.event.isInstanceOf[OrderLockReleased]).get
    val bReleased = stampedEvents.find(stamped => stamped.value.key == aOrderId && stamped.value.event.isInstanceOf[OrderLockReleased]).get
    assert(aAquired.eventId < bReleased.eventId, "- a acquired lock after b released")
    assert(bAquired.eventId < aReleased.eventId, "- b acquired lock after a released")
    assert(controllerState.keyTo(LockState)(limit2LockPath) ==
      LockState(
        Lock(limit2LockPath, limit = 2, itemRevision = Some(ItemRevision(0))),
        Available,
        Queue.empty))
  }

  "Multiple orders with count=1 and count=2 finish" in {
    val workflow1 = defineWorkflow(workflow1Path, s"""
      define workflow {
        lock (lock="${limit2LockPath.string}", count=1) {
          execute agent="AGENT", script="${script(10.ms)}", parallelism=99
        }
      }""")
    val workflow2 = defineWorkflow(workflow2Path, s"""
      define workflow {
        lock (lock="${limit2LockPath.string}", count=2) {
          execute agent="AGENT", script="${script(10.ms)}", parallelism=99
        }
      }""")
    val orders = Random.shuffle(
      for (workflow <- Seq(workflow1, workflow2); i <- 1 to 100) yield
        FreshOrder(OrderId(s"${workflow.path.string}-$i"), workflow.path))
    controllerApi.addOrders(Observable.from(orders)).await(99.s).orThrow
    controllerApi.executeCommand(DeleteOrdersWhenTerminated(orders.map(_.id))).await(99.s).orThrow
    val terminated = for (order <- orders) yield controller.eventWatch.await[OrderTerminated](_.key == order.id)
    for (keyedEvent <- terminated.map(_.last.value))  assert(keyedEvent.event == OrderFinished, s"- ${keyedEvent.key}")
    for (order <- orders) controller.eventWatch.await[OrderDeleted](_.key == order.id)
    assert(controllerState.keyTo(LockState)(limit2LockPath) ==
      LockState(
        Lock(limit2LockPath, limit = 2, itemRevision = Some(ItemRevision(0))),
        Available,
        Queue.empty))
  }

  "Failed order" in {
    val workflow = updateItem(Workflow(
      WorkflowPath("FINISH-IN-FORK"),
      Seq(
        EmptyJob.execute(agentPath),  // Complicate with a Lock at non-first position
        LockInstruction(
          lockPath,
          count = None,
          lockedWorkflow = Workflow.of(
            EmptyJob.execute(agentPath),  // Complicate with a Lock at non-first position
            LockInstruction(
              lock2Path,
              count = None,
              lockedWorkflow = Workflow.of(
                Fail())))))))

    assert(workflow.referencedItemPaths.toSet == Set(lockPath, lock2Path, agentPath))

    val orderId = OrderId("🟦")
    controllerApi.addOrder(FreshOrder(orderId, workflow.path, deleteWhenTerminated = true))
      .await(99.s).orThrow
    controller.eventWatch.await[OrderFailed](_.key == orderId)

    controllerApi.executeCommand(CancelOrders(Seq(orderId))).await(99.s).orThrow
    controller.eventWatch.await[OrderDeleted](_.key == orderId)

    assert(controller.eventWatch.eventsByKey[OrderEvent](orderId) == Seq(
      OrderAdded(workflow.id, deleteWhenTerminated = true),

      OrderAttachable(agentPath),
      OrderAttached(agentPath),
      OrderStarted,
      OrderProcessingStarted(subagentId),
      OrderProcessed(Outcome.succeeded),
      OrderMoved(Position(1)),
      OrderDetachable,
      OrderDetached,

      OrderLockAcquired(lockPath, None),

      OrderAttachable(agentPath),
      OrderAttached(agentPath),
      OrderProcessingStarted(subagentId),
      OrderProcessed(Outcome.succeeded),
      OrderMoved(Position(1) / "lock" % 1),
      OrderDetachable,
      OrderDetached,

      OrderLockAcquired(lock2Path, None),
      OrderLockReleased(lock2Path),
      OrderLockReleased(lockPath),
      OrderFailed(Position(1), Some(Outcome.failed)),
      OrderCancelled,
      OrderDeleted))

    assert(controllerState.keyTo(LockState)(lockPath) ==
      LockState(
        Lock(lockPath, itemRevision = Some(ItemRevision(0))),
        Available, Queue.empty))
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
    assert(workflow.referencedItemPaths.toSet == Set(lockPath, lock2Path, agentPath))

    val orderId = OrderId("🟨")
    controllerApi.addOrder(FreshOrder(orderId, workflow.path, deleteWhenTerminated = true))
      .await(99.s).orThrow
    controller.eventWatch.await[OrderDeleted](_.key == orderId)
    assert(controller.eventWatch.eventsByKey[OrderEvent](orderId) == Seq(
      OrderAdded(workflow.id, deleteWhenTerminated = true),
      OrderStarted,
      OrderLockAcquired(lockPath, None),
      OrderMoved(Position(0) / "lock" % 0 / "try+0" % 0),
      OrderLockAcquired(lock2Path, None),
      OrderLockReleased(lock2Path),
      OrderCaught(Position(0) / "lock" % 0 / "catch+0" % 0, Some(Outcome.failed)),
      OrderAttachable(agentPath),
      OrderAttached(agentPath),
      OrderProcessingStarted(subagentId),
      OrderProcessed(Outcome.succeededRC0),
      OrderMoved(Position(0) / "lock" % 1),
      OrderDetachable,
      OrderDetached,
      OrderLockReleased(lockPath),
      OrderFinished,
      OrderDeleted))
    assert(controllerState.keyTo(LockState)(lockPath) ==
      LockState(
        Lock(lockPath, itemRevision = Some(ItemRevision(0))),
        Available,
        Queue.empty))
  }

  "Failed forked order" in {
    val workflow = defineWorkflow(workflowNotation = """
      define workflow {
        fork (joinIfFailed=true) {
          "BRANCH": {
            lock (lock = "LOCK") {
              fail;
            }
          }
        }
      }""")
    assert(workflow.referencedItemPaths.toSet == Set(lockPath))

    val orderId = OrderId("🟩")
    controllerApi.addOrder(FreshOrder(orderId, workflow.path, deleteWhenTerminated = true))
      .await(99.s).orThrow

    controller.eventWatch.await[OrderFailed](_.key == orderId)
    controllerApi.executeCommand(DeleteOrdersWhenTerminated(Seq(orderId))).await(99.s).orThrow
    controllerApi.executeCommand(CancelOrders(Seq(orderId))).await(99.s).orThrow
    assert(controller.eventWatch.eventsByKey[OrderEvent](orderId) == Seq(
      OrderAdded(workflow.id, deleteWhenTerminated = true),
      OrderStarted,
      OrderForked(Vector(OrderForked.Child("BRANCH", orderId / "BRANCH"))),
      OrderJoined(Outcome.Failed(Some("Order:🟩|BRANCH failed"))),
      OrderFailed(Position(0)),
      OrderCancelled,
      OrderDeleted))

    assert(controller.eventWatch.eventsByKey[OrderEvent](orderId / "BRANCH") == Seq(
      OrderLockAcquired(lockPath, None),
      OrderLockReleased(lockPath),
      OrderFailedInFork(Position(0) / "fork+BRANCH" % 0, Some(Outcome.failed))))

    assert(controllerState.keyTo(LockState)(lockPath) ==
      LockState(
        Lock(lockPath, itemRevision = Some(ItemRevision(0))),
        Available,
        Queue.empty))
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
    controllerApi.addOrder(FreshOrder(orderId, workflow.path, deleteWhenTerminated = true))
      .await(99.s).orThrow

    controller.eventWatch.await[OrderFailed](_.key == orderId / "BRANCH")
    controllerApi.executeCommand(CancelOrders(Seq(orderId / "BRANCH", orderId)))
      .await(99.s).orThrow
    assert(controller.eventWatch.eventsByKey[OrderEvent](orderId) == Seq(
      OrderAdded(workflow.id, deleteWhenTerminated = true),
      OrderStarted,
      OrderLockAcquired(lockPath, None),
      OrderForked(Vector(OrderForked.Child("BRANCH", orderId / "BRANCH"))),
      OrderCancellationMarked(),
      OrderJoined(Outcome.Failed(Some("Order:🟪|BRANCH has been cancelled"))),
      OrderLockReleased(lockPath),
      OrderFailed(Position(0)),
      OrderCancelled,
      OrderDeleted))

    assert(controller.eventWatch.eventsByKey[OrderEvent](orderId / "BRANCH") == Seq(
      OrderFailed(Position(0) / "lock" % 0 / "fork+BRANCH" % 0, Some(Outcome.Disrupted(Problem(
        "Lock:LOCK has already been acquired by parent Order:🟪")))),
      OrderCancelled))

    assert(controllerState.keyTo(LockState)(lockPath) ==
      LockState(
        Lock(lockPath, itemRevision = Some(ItemRevision(0))),
        Available,
        Queue.empty))
  }

  "Finish instruction" in {
    val workflow = updateItem(Workflow(
      WorkflowPath("FINISH"),
      Seq(
        EmptyJob.execute(agentPath),  // Complicate with a Lock at non-first position
        LockInstruction(
          lockPath,
          count = None,
          lockedWorkflow = Workflow.of(Finish())))))

    val orderId = OrderId("🟫")
    controllerApi.addOrder(FreshOrder(orderId, workflow.path, deleteWhenTerminated = true))
      .await(99.s).orThrow
    controller.eventWatch.await[OrderFinished](_.key == orderId)

    assert(controller.eventWatch.eventsByKey[OrderEvent](orderId) == Seq(
      OrderAdded(workflow.id, deleteWhenTerminated = true),

      OrderAttachable(agentPath),
      OrderAttached(agentPath),
      OrderStarted,
      OrderProcessingStarted(subagentId),
      OrderProcessed(Outcome.succeeded),
      OrderMoved(Position(1)),
      OrderDetachable,
      OrderDetached,

      OrderLockAcquired(lockPath, None),
      OrderLockReleased(lockPath),
      OrderFinished,
      OrderDeleted))

    assert(controllerState.keyTo(LockState)(lockPath) ==
      LockState(
        Lock(lockPath, itemRevision = Some(ItemRevision(0))),
        Available,
        Queue.empty))

    controllerApi
      .updateItems(Observable(
        AddVersion(VersionId("FINISH-REMOVED")),
        RemoveVersioned(workflow.path)))
      .await(99.s).orThrow
  }

  "Finish instruction in Fork" in {
    val workflow = updateItem(Workflow(
      WorkflowPath("FINISH-IN-FORK"),
      Seq(
        Fork.forTest(Seq(Fork.Branch(
          Fork.Branch.Id("BRANCH"),
          Workflow.of(
            LockInstruction(
              lockPath,
              count = None,
              lockedWorkflow = Workflow.of(Finish())))))))))

    val orderId = OrderId("🟣")
    controllerApi.addOrder(FreshOrder(orderId, workflow.path, deleteWhenTerminated = true))
      .await(99.s).orThrow
    controller.eventWatch.await[OrderFinished](_.key == orderId)

    assert(controller.eventWatch.eventsByKey[OrderEvent](orderId / "BRANCH") == Seq(
      OrderLockAcquired(lockPath, None),
      OrderLockReleased(lockPath),
      OrderMoved(Position(0) / BranchId.fork("BRANCH") % 1)))

    assert(controllerState.keyTo(LockState)(lockPath) ==
      LockState(
        Lock(lockPath, itemRevision = Some(ItemRevision(0))),
        Available,
        Queue.empty))

    controllerApi
      .updateItems(Observable(
        AddVersion(VersionId("FINISH-IN-FORK-REMOVED")),
        RemoveVersioned(workflow.path)))
      .await(99.s).orThrow
  }

  "Acquire too much" in {
    val workflow = defineWorkflow(workflowNotation = """
      define workflow {
        lock (lock = "LOCK", count = 2) {}
      }""")
    val orderId = OrderId("⬛️")
    controllerApi.addOrder(FreshOrder(orderId, workflow.path, deleteWhenTerminated = true))
      .await(99.s).orThrow

    controller.eventWatch.await[OrderFailed](_.key == orderId)
    controllerApi.executeCommand(CancelOrders(Seq(orderId))).await(99.s).orThrow
    assert(controller.eventWatch.eventsByKey[OrderEvent](orderId) == Seq(
      OrderAdded(workflow.id, deleteWhenTerminated = true),
      OrderStarted,
      OrderFailed(Position(0), Some(Outcome.Disrupted(Problem("Cannot fulfill lock count=2 with Lock:LOCK limit=1")))),
      OrderCancelled,
      OrderDeleted))

    assert(controllerState.keyTo(LockState)(lockPath) ==
      LockState(
        Lock(lockPath, itemRevision = Some(ItemRevision(0))),
        Available,
        Queue.empty))
  }

  "Cancel while waiting for lock" in {
    val lockingOrderId = OrderId("CANCEL-WHILE-QUEUING-IN-LOCK")
    controllerApi
      .addOrder(FreshOrder(lockingOrderId, promptInLockWorkflow.path, deleteWhenTerminated = true))
      .await(99.s).orThrow
    controller.eventWatch.await[OrderPrompted](_.key == lockingOrderId)

    val queueingWorkflow = updateItem(Workflow(
      WorkflowPath("CANCEL-WHILE-QUEUING-FOR-LOCKING"),
      Seq(
        LockInstruction(lockPath,
          count = None,
          lockedWorkflow = Workflow.empty))))
    val orderId = OrderId("CANCEL-WHILE-QUEUING-FOR-LOCK")
    controllerApi.addOrder(FreshOrder(orderId, queueingWorkflow.path, deleteWhenTerminated = true))
      .await(99.s).orThrow
    controller.eventWatch.await[OrderLockQueued](_.key == orderId)

    controllerApi.executeCommand(CancelOrders(Seq(orderId))).await(99.s).orThrow
    controller.eventWatch.await[OrderCancelled](_.key == orderId)

    assert(controller.eventWatch.eventsByKey[OrderEvent](orderId) == Seq(
      OrderAdded(queueingWorkflow.id, deleteWhenTerminated = true),
      OrderStarted,
      OrderLockQueued(lockPath, count = None),
      OrderLockDequeued(lockPath),
      OrderCancelled,
      OrderDeleted))

    controllerApi.executeCommand(AnswerOrderPrompt(lockingOrderId)).await(99.s).orThrow
    controller.eventWatch.await[OrderDeleted](_.key == lockingOrderId)
    assert(controller.eventWatch.eventsByKey[OrderEvent](lockingOrderId) == Seq(
      OrderAdded(promptInLockWorkflow.id, deleteWhenTerminated = true),
      OrderStarted,
      OrderLockAcquired(lockPath),
      OrderPrompted(StringValue("PROMPT")),
      OrderPromptAnswered(),
      OrderMoved(Position(0) / "lock" % 1),
      OrderLockReleased(lockPath),
      OrderFinished,
      OrderDeleted))

    assert(controllerState.keyTo(LockState)(lockPath) ==
      LockState(
        Lock(lockPath, itemRevision = Some(ItemRevision(0))),
        Available,
        Queue.empty))

    controllerApi.updateItems(Observable(
      AddVersion(VersionId("DELETE")),
      RemoveVersioned(queueingWorkflow.path)
    )).await(99.s).orThrow
  }

  "Cancel while using a lock" in {
    val orderId = OrderId("CANCEL-WHILE-IN-LOCK")
    controllerApi
      .addOrder(FreshOrder(orderId, promptInLockWorkflow.path, deleteWhenTerminated = true))
      .await(99.s).orThrow
    controller.eventWatch.await[OrderPrompted](_.key == orderId)

    controllerApi.executeCommand(CancelOrders(Seq(orderId))).await(99.s).orThrow
    assert(controller.eventWatch.eventsByKey[OrderEvent](orderId) == Seq(
      OrderAdded(promptInLockWorkflow.id, deleteWhenTerminated = true),
      OrderStarted,
      OrderLockAcquired(lockPath),
      OrderPrompted(StringValue("PROMPT")),
      OrderLockReleased(lockPath),
      OrderCancelled,
      OrderDeleted))

    assert(controllerState.keyTo(LockState)(lockPath) ==
      LockState(
        Lock(lockPath, itemRevision = Some(ItemRevision(0))),
        Available,
        Queue.empty))
  }

  "Use Lock both exclusivly and non-exclusively" in {
    val exclusiveWorkflow = updateItem(Workflow(
      WorkflowPath("EXCLUSIVE-WORKFLOW"),
      Seq(
        LockInstruction(limit2LockPath,
          count = None,
          lockedWorkflow = Workflow.of(Prompt(expr("'?'")))))))
    val nonExclusiveWorkflow = updateItem(Workflow(
      WorkflowPath("NON-EXCLUSIVE-WORKFLOW"),
      Seq(
        LockInstruction(limit2LockPath,
          count = Some(2),
          lockedWorkflow = Workflow.of(Prompt(expr("'?'")))))))

    locally {
      // First exclusive, then non-exclusive is queued
      val aOrder = FreshOrder(OrderId("EXCLUSIVE-A"), exclusiveWorkflow.path,
        deleteWhenTerminated = true)
      controllerApi.addOrder(aOrder).await(99.s).orThrow
      eventWatch.await[OrderPrompted](_.key == aOrder.id)

      val bOrder = FreshOrder(OrderId("EXCLUSIVE-B"), nonExclusiveWorkflow.path,
        deleteWhenTerminated = true)
      controllerApi.addOrder(bOrder).await(99.s).orThrow
      eventWatch.await[OrderLockQueued](_.key == bOrder.id)

      controllerApi.executeCommand(AnswerOrderPrompt(aOrder.id)).await(99.s).orThrow
      eventWatch.await[OrderFinished](_.key == aOrder.id)

      eventWatch.await[OrderPrompted](_.key == bOrder.id)
      controllerApi.executeCommand(AnswerOrderPrompt(bOrder.id)).await(99.s).orThrow
      eventWatch.await[OrderFinished](_.key == bOrder.id)
    }

    locally {
      // First non-exclusive, then exclusive is queued
      val aOrder = FreshOrder(OrderId("EXCLUSIVE-AA"), exclusiveWorkflow.path,
        deleteWhenTerminated = true)
      controllerApi.addOrder(aOrder).await(99.s).orThrow
      eventWatch.await[OrderPrompted](_.key == aOrder.id)

      val bOrder = FreshOrder(OrderId("EXCLUSIVE-BB"), nonExclusiveWorkflow.path,
        deleteWhenTerminated = true)
      controllerApi.addOrder(bOrder).await(99.s).orThrow
      eventWatch.await[OrderLockQueued](_.key == bOrder.id)

      controllerApi.executeCommand(AnswerOrderPrompt(aOrder.id)).await(99.s).orThrow
      eventWatch.await[OrderFinished](_.key == aOrder.id)
      eventWatch.await[OrderDeleted](_.key == aOrder.id)

      eventWatch.await[OrderPrompted](_.key == bOrder.id)
      controllerApi.executeCommand(AnswerOrderPrompt(bOrder.id)).await(99.s).orThrow
      eventWatch.await[OrderFinished](_.key == bOrder.id)
      eventWatch.await[OrderDeleted](_.key == bOrder.id)
    }

    controllerApi.updateItems(Observable(
      AddVersion(VersionId("REMOVE-EXCLUSIVE")),
      RemoveVersioned(exclusiveWorkflow.path),
      RemoveVersioned(nonExclusiveWorkflow.path)
    )).await(99.s).orThrow
  }

  "Lock is not deletable while in use by a Workflow" in {
    val workflow = defineWorkflow(workflowNotation = """
      define workflow {
        lock (lock = "LOCK") {}
      }""")
    val v = VersionId("DELETE-BUT-ORDER")
    assert(controllerApi.updateItems(Observable(
      AddVersion(v),
      DeleteSimple(lockPath)
    )).await(99.s) == Left(Problem.Combined(Set(
      ItemIsStillReferencedProblem(lockPath, promptInLockWorkflow.id),
      ItemIsStillReferencedProblem(lockPath, workflow.id)))))
  }

  "Lock is not deletable while in use by a deleted Workflow with a still running order" in {
    val workflow = defineWorkflow(workflowNotation = """
      define workflow {
        lock (lock = "LOCK") {
          prompt "LOCKED";
        }
      }""")
    val v = VersionId("DELETE-WHILE-ORDER")
    val orderId = OrderId("DELETING")
    controllerApi.addOrder(FreshOrder(orderId, workflow.path, deleteWhenTerminated = true))
      .await(99.s).orThrow
    controller.eventWatch.await[OrderPrompted](_.key == orderId)

    def removeWorkflowAndLock() = controllerApi
      .updateItems(Observable(
        DeleteSimple(lockPath),
        AddVersion(v),
        RemoveVersioned(workflow.path),
        RemoveVersioned(promptInLockWorkflow.path)))
      .await(99.s)
    // Lock cannot be deleted due to orders in the deleted (but still versioned) Workflow
    assert(removeWorkflowAndLock() == Left(ItemIsStillReferencedProblem(lockPath, workflow.id)))

    controllerApi.executeCommand(AnswerOrderPrompt(orderId)).await(99.s).orThrow
    controller.eventWatch.await[OrderDeleted](_.key == orderId)
    removeWorkflowAndLock().orThrow
  }

  "Remove Workflow and Lock" in {
    val eventId = controller.eventWatch.lastAddedEventId
    val previousControllerState = controllerState
    val v = VersionId("DELETE")
    controllerApi.updateItems(Observable(
      DeleteSimple(lockPath),
      DeleteSimple(lock2Path),
      DeleteSimple(limit2LockPath),
      AddVersion(v),
      RemoveVersioned(workflowPath),
      RemoveVersioned(workflow1Path),
      RemoveVersioned(workflow2Path)
    )).await(99.s).orThrow
    for (workflowId <- previousControllerState.repo.itemIdsFor(WorkflowPath)) {
      controller.eventWatch.await[ItemDeleted](_.event.key == workflowId, after = eventId)
    }
    previousControllerState.itemToAgentToAttachedState
      .foreach {
        case (WorkflowId.as(id), _) =>
          controller.eventWatch.await[ItemDetached](_.event.key == id, after = eventId)
        case _ =>
      }
    controller.eventWatch.await[ItemDeleted](_.event.key == lockPath, after = eventId)
    controller.eventWatch.await[ItemDeleted](_.event.key == lock2Path, after = eventId)
    controller.eventWatch.await[ItemDeleted](_.event.key == limit2LockPath, after = eventId)

    assert(controllerState.idToOrder.isEmpty)
    assert(controllerState.repo == Repo.empty
      .applyEvents(controllerState.repo.versionIds.reverse.map(VersionAdded(_)))
      .orThrow)
    assert(controllerState.keyTo(LockState).isEmpty)

    // No Lock is attached to any Agent
    assert(controllerState.itemToAgentToAttachedState == Map(
      SubagentId("AGENT-0") -> Map(
        AgentPath("AGENT") -> Attached(Some(ItemRevision(0)))),
      SubagentId("B-AGENT-0") -> Map(
        AgentPath("B-AGENT") -> Attached(Some(ItemRevision(0))))))
  }

  private def defineWorkflow(workflowNotation: String): Workflow =
    defineWorkflow(workflowPath, workflowNotation)

  private def defineWorkflow(workflowPath: WorkflowPath, workflowNotation: String): Workflow = {
    val workflow = WorkflowParser.parse(workflowPath, workflowNotation).orThrow
    updateItem(workflow)
  }
}

object LockTest
{
  private val agentPath = AgentPath("AGENT")
  private val subagentId = toLocalSubagentId(agentPath)
  private val bAgentPath = AgentPath("B-AGENT")
  private val bSubagentId = toLocalSubagentId(bAgentPath)
  private val workflowPath = WorkflowPath("WORKFLOW")
  private val workflow1Path = WorkflowPath("WORKFLOW-1")
  private val workflow2Path = WorkflowPath("WORKFLOW-2")
  private val lockPath = LockPath("LOCK")
  private val lock2Path = LockPath("LOCK-2")
  private val limit2LockPath = LockPath("LOCK-LIMIT-2")

  private val promptInLockWorkflow = Workflow(
    WorkflowPath("PROMPT-IN-LOCK") ~ "INITIAL",
    Seq(
      LockInstruction(lockPath,
        count = None,
        lockedWorkflow = Workflow.of(
          Prompt(StringConstant("PROMPT"))))))
}
