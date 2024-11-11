package js7.tests

import fs2.Stream
import java.nio.file.Files.delete
import js7.base.configutils.Configs.*
import js7.base.io.file.FileUtils.{touchFile, withTemporaryFile}
import js7.base.problem.Checked.Ops
import js7.base.problem.Problem
import js7.base.test.OurTestSuite
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.time.Timestamp
import js7.data.Problems.ItemIsStillReferencedProblem
import js7.data.agent.AgentPath
import js7.data.controller.ControllerCommand.{AnswerOrderPrompt, CancelOrders, DeleteOrdersWhenTerminated, TransferOrders}
import js7.data.item.BasicItemEvent.{ItemDeleted, ItemDetached}
import js7.data.item.ItemAttachedState.Attached
import js7.data.item.ItemOperation.{AddVersion, DeleteSimple, RemoveVersioned}
import js7.data.item.VersionedEvent.VersionAdded
import js7.data.item.{ItemRevision, Repo, VersionId}
import js7.data.lock.Acquired.{Available, Exclusive}
import js7.data.lock.{Lock, LockPath, LockState}
import js7.data.order.OrderEvent.*
import js7.data.order.{FreshOrder, OrderEvent, OrderId, OrderOutcome}
import js7.data.subagent.SubagentId
import js7.data.value.StringValue
import js7.data.value.ValuePrinter.quoteString
import js7.data.value.expression.Expression.{NumericConstant, StringConstant}
import js7.data.value.expression.ExpressionParser.expr
import js7.data.workflow.instructions.{Fail, Finish, Fork, ForkBranchId, LockInstruction, Options, Prompt, Retry, TryInstruction}
import js7.data.workflow.position.BranchPath.syntax.*
import js7.data.workflow.position.{BranchId, Position}
import js7.data.workflow.{Workflow, WorkflowId, WorkflowParser, WorkflowPath}
import js7.tests.LockTest.*
import js7.tests.jobs.{EmptyJob, FailingJob, SemaphoreJob, SleepJob}
import js7.tests.testenv.DirectoryProvider.{toLocalSubagentId, waitingForFileScript}
import js7.tests.testenv.{BlockingItemUpdater, ControllerAgentForScalaTest}
import scala.collection.immutable.Queue
import scala.util.Random

final class LockTest extends OurTestSuite, ControllerAgentForScalaTest, BlockingItemUpdater:

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

  "Run some orders at different agents with a lock with limit=1" in:
    withTemporaryFile("LockTest-", ".tmp") { file =>
      val workflow = defineWorkflow(s"""
        define workflow {
          lock (lock = "LOCK") {
            job `JOB-1`;
            job `JOB-2`;
          };
          define job `JOB-1` {
            execute agent="${agentPath.string}", script=${quoteString(waitingForFileScript(file))}, processLimit = 100;
          }
          define job `JOB-2` {
            execute agent="${bAgentPath.string}", script=${quoteString(waitingForFileScript(file))}, processLimit = 100;
          }
        }""")
      assert(workflow.referencedItemPaths.toSet == Set(lockPath, agentPath, bAgentPath))

      delete(file)
      val a = OrderId("🔷")
      controller.api.addOrder(FreshOrder(a, workflow.path, deleteWhenTerminated = true))
        .await(99.s).orThrow
      assert(controller.eventWatch.await[OrderLocksAcquired](_.key == a).map(_.value).nonEmpty)

      val queuedOrderIds = for i <- 1 to 10 yield OrderId(s"🔶-$i")
      for orderId <- queuedOrderIds do
        controller.api.addOrder(FreshOrder(orderId, workflow.path, deleteWhenTerminated = true))
          .await(99.s).orThrow
        assert(controller.eventWatch.await[OrderLocksQueued](_.key == orderId).map(_.value).nonEmpty)

      touchFile(file)

      assert(controller.eventWatch.await[OrderTerminated](_.key == a).map(_.value) == Seq(a <-: OrderFinished()))
      controller.eventWatch.await[OrderDeleted](_.key == a)
      assert(controller.eventWatch.eventsByKey[OrderEvent](a) == Seq(
        OrderAdded(workflow.id, deleteWhenTerminated = true),
        OrderStarted,
        OrderLocksAcquired(List(LockDemand(lockPath, None))),
          OrderAttachable(agentPath),
          OrderAttached(agentPath),
          OrderProcessingStarted(subagentId),
          OrderProcessed(OrderOutcome.succeededRC0),
          OrderMoved(Position(0) / "lock" % 1),
          OrderDetachable,
          OrderDetached,

          OrderAttachable(bAgentPath),
          OrderAttached(bAgentPath),
          OrderProcessingStarted(bSubagentId),
          OrderProcessed(OrderOutcome.succeededRC0),
          OrderMoved(Position(0) / "lock" % 2),
          OrderDetachable,
          OrderDetached,
        OrderLocksReleased(List(lockPath)),
        OrderFinished(),
        OrderDeleted))

      for orderId <- queuedOrderIds do
        assert(controller.eventWatch.await[OrderTerminated](_.key == orderId).map(_.value) == Seq(orderId <-: OrderFinished()))
        controller.eventWatch.await[OrderDeleted](_.key == orderId)
        assert(controller.eventWatch.eventsByKey[OrderEvent](orderId) == Seq(
          OrderAdded(workflow.id, deleteWhenTerminated = true),
          OrderStarted,
          OrderLocksQueued(List(LockDemand(lockPath))),
          OrderLocksAcquired(List(LockDemand(lockPath))),
            OrderAttachable(agentPath),
            OrderAttached(agentPath),
            OrderProcessingStarted(subagentId),
            OrderProcessed(OrderOutcome.succeededRC0),
            OrderMoved(Position(0) / "lock" % 1),
            OrderDetachable,
            OrderDetached,

            OrderAttachable(bAgentPath),
            OrderAttached(bAgentPath),
            OrderProcessingStarted(bSubagentId),
            OrderProcessed(OrderOutcome.succeededRC0),
            OrderMoved(Position(0) / "lock" % 2),
            OrderDetachable,
            OrderDetached,
          OrderLocksReleased(List(lockPath)),
          OrderFinished(),
          OrderDeleted))

      for pair <- queuedOrderIds.sliding(2).toSeq do
        assert(controller.eventWatch.await[OrderLocksAcquired](_.key == pair(0)).map(_.eventId).head <
               controller.eventWatch.await[OrderLocksAcquired](_.key == pair(1)).map(_.eventId).head)
      assert(controllerState.keyTo(LockState)(lockPath) ==
        LockState(
          Lock(lockPath, itemRevision = Some(ItemRevision(0))),
          Available,
          Queue.empty))
    }

  "After releasing a lock of 2, two orders with count=1 each start simultaneously" in:
    val workflow1 = updateItem(Workflow(
      WorkflowPath("RELEASE-2"),
      Seq:
        LockInstruction.single(
          limit2LockPath,
          count = Some(1),
          lockedWorkflow = Workflow.of:
            Prompt(expr("'PROMPT'")))))

    val workflow2 = updateItem(Workflow(
      WorkflowPath("WORKFLOW-2"),
      Seq:
        LockInstruction.single(
          limit2LockPath,
          count = Some(2),
          lockedWorkflow = Workflow.of:
            Prompt(expr("'PROMPT'")))))

    val order2Id = OrderId("🟥-TWO")
    val aOrderId = OrderId("🟥-A")
    val bOrderId = OrderId("🟥-B")
    val cOrderId = OrderId("🟥-C")
    controller.api.addOrder(FreshOrder(order2Id, workflow2.path, deleteWhenTerminated = true))
      .await(99.s).orThrow
    controller.eventWatch.awaitNext[OrderLocksAcquired](_.key == order2Id)

    for orderId <- Seq(aOrderId, bOrderId, cOrderId) do
      controller.api.addOrder(FreshOrder(orderId, workflow1.path, deleteWhenTerminated = true))
        .await(99.s).orThrow
      controller.eventWatch.awaitNext[OrderLocksQueued](_.key == orderId)

    controller.api.executeCommand(AnswerOrderPrompt(order2Id)).await(99.s).orThrow
    controller.eventWatch.awaitNext[OrderLocksReleased](_.key == order2Id)

    controller.eventWatch.await[OrderLocksAcquired](_.key == aOrderId)
    controller.eventWatch.await[OrderLocksAcquired](_.key == bOrderId)
    controller.eventWatch.await[OrderPrompted](_.key == aOrderId)
    controller.eventWatch.await[OrderPrompted](_.key == bOrderId)
    controller.api.executeCommand(AnswerOrderPrompt(aOrderId)).await(99.s).orThrow
    controller.api.executeCommand(AnswerOrderPrompt(bOrderId)).await(99.s).orThrow

    controller.eventWatch.awaitNext[OrderLocksAcquired](_.key == cOrderId)
    controller.api.executeCommand(AnswerOrderPrompt(cOrderId)).await(99.s).orThrow

    controller.eventWatch.await[OrderTerminated](_.key == aOrderId)
    controller.eventWatch.await[OrderTerminated](_.key == bOrderId)
    controller.eventWatch.await[OrderTerminated](_.key == cOrderId)

    assert(controllerState.keyTo(LockState)(limit2LockPath) ==
      LockState(
        Lock(limit2LockPath, limit = 2, itemRevision = Some(ItemRevision(0))),
        Available,
        Queue.empty))

    deleteItems(workflow1.path, workflow2.path)

  "Multiple orders with count=1 and count=2 finish" in:
    val workflow1 = updateItem(Workflow.of(workflow1Path,
      LockInstruction(
        Seq(LockDemand(limit2LockPath, count = Some(1))),
        Workflow.of(
          SleepJob.execute(
            agentPath,
            arguments = Map(
              "sleep" -> NumericConstant(10.ms.toBigDecimalSeconds)),
            processLimit = 99)))))

    val workflow2 = updateItem(Workflow.of(workflow2Path,
      LockInstruction(
        Seq(LockDemand(limit2LockPath, count = Some(2))),
        Workflow.of(
          SleepJob.execute(
            agentPath,
            arguments = Map(
              "sleep" -> NumericConstant(10.ms.toBigDecimalSeconds)),
            processLimit = 99)))))

    val orders = Random.shuffle(
      for workflow <- Seq(workflow1, workflow2); i <- 1 to 100 yield
        FreshOrder(OrderId(s"${workflow.path.string}-$i"), workflow.path))
    controller.api.addOrders(Stream.iterable(orders)).await(99.s).orThrow
    controller.api.executeCommand(DeleteOrdersWhenTerminated(orders.map(_.id))).await(99.s).orThrow
    val terminated = for order <- orders yield controller.eventWatch.await[OrderTerminated](_.key == order.id)
    val terminatedX = controller.eventWatch.awaitKeys[OrderTerminated](orders.map(_.id))
    for keyedEvent <- terminated.map(_.last.value) do  assert(keyedEvent.event == OrderFinished(), s"- ${keyedEvent.key}")
    controller.eventWatch.awaitKeys[OrderDeleted](orders.map(_.id))
    assert(controllerState.keyTo(LockState)(limit2LockPath) ==
      LockState(
        Lock(limit2LockPath, limit = 2, itemRevision = Some(ItemRevision(0))),
        Available,
        Queue.empty))

  "Failed order" in:
    val workflow = updateItem(Workflow(
      WorkflowPath("FAILED-ORDER"),
      Seq(
        EmptyJob.execute(agentPath),  // Complicate with a Lock at non-first position
        LockInstruction.single(
          lockPath,
          count = None,
          lockedWorkflow = Workflow.of(
            EmptyJob.execute(agentPath),  // Complicate with a Lock at non-first position
            LockInstruction.single(
              lock2Path,
              count = None,
              lockedWorkflow = Workflow.of(
                Fail())))))))

    assert(workflow.referencedItemPaths.toSet == Set(lockPath, lock2Path, agentPath))

    val orderId = OrderId("🟦")
    controller.api.addOrder(FreshOrder(orderId, workflow.path, deleteWhenTerminated = true))
      .await(99.s).orThrow
    controller.eventWatch.await[OrderFailed](_.key == orderId)

    controller.api.executeCommand(CancelOrders(Seq(orderId))).await(99.s).orThrow
    controller.eventWatch.await[OrderDeleted](_.key == orderId)

    assert(controller.eventWatch.eventsByKey[OrderEvent](orderId) == Seq(
      OrderAdded(workflow.id, deleteWhenTerminated = true),

      OrderAttachable(agentPath),
      OrderAttached(agentPath),
      OrderStarted,
      OrderProcessingStarted(subagentId),
      OrderProcessed(OrderOutcome.succeeded),
      OrderMoved(Position(1)),
      OrderDetachable,
      OrderDetached,

      OrderLocksAcquired(List(LockDemand(lockPath))),

      OrderAttachable(agentPath),
      OrderAttached(agentPath),
      OrderProcessingStarted(subagentId),
      OrderProcessed(OrderOutcome.succeeded),
      OrderMoved(Position(1) / "lock" % 1),
      OrderDetachable,
      OrderDetached,

      OrderLocksAcquired(List(LockDemand(lock2Path))),
      OrderOutcomeAdded(OrderOutcome.failed),
      OrderLocksReleased(List(lock2Path)),
      OrderLocksReleased(List(lockPath)),
      OrderFailed(Position(1)),
      OrderCancelled,
      OrderDeleted))

    assert(controllerState.keyTo(LockState)(lockPath) ==
      LockState(
        Lock(lockPath, itemRevision = Some(ItemRevision(0))),
        Available, Queue.empty))

    deleteItems(workflow.path)

  "Failed order in try/catch" in:
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
    controller.api.addOrder(FreshOrder(orderId, workflow.path, deleteWhenTerminated = true))
      .await(99.s).orThrow
    controller.eventWatch.await[OrderDeleted](_.key == orderId)
    assert(controller.eventWatch.eventsByKey[OrderEvent](orderId) == Seq(
      OrderAdded(workflow.id, deleteWhenTerminated = true),
      OrderStarted,
      OrderLocksAcquired(List(LockDemand(lockPath))),
      OrderMoved(Position(0) / "lock" % 0 / "try+0" % 0),
      OrderLocksAcquired(List(LockDemand(lock2Path))),
      OrderOutcomeAdded(OrderOutcome.failed),
      OrderLocksReleased(List(lock2Path)),
      OrderCaught(Position(0) / "lock" % 0 / "catch+0" % 0),
      OrderAttachable(agentPath),
      OrderAttached(agentPath),
      OrderProcessingStarted(subagentId),
      OrderProcessed(OrderOutcome.succeededRC0),
      OrderMoved(Position(0) / "lock" % 1),
      OrderDetachable,
      OrderDetached,
      OrderLocksReleased(List(lockPath)),
      OrderFinished(),
      OrderDeleted))
    assert(controllerState.keyTo(LockState)(lockPath) ==
      LockState(
        Lock(lockPath, itemRevision = Some(ItemRevision(0))),
        Available,
        Queue.empty))

  "Failed forked order" in:
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
    controller.api.addOrder(FreshOrder(orderId, workflow.path, deleteWhenTerminated = true))
      .await(99.s).orThrow

    controller.eventWatch.await[OrderFailed](_.key == orderId)
    controller.api.executeCommand(DeleteOrdersWhenTerminated(Seq(orderId))).await(99.s).orThrow
    controller.api.executeCommand(CancelOrders(Seq(orderId))).await(99.s).orThrow
    assert(controller.eventWatch.eventsByKey[OrderEvent](orderId) == Seq(
      OrderAdded(workflow.id, deleteWhenTerminated = true),
      OrderStarted,
      OrderForked(Vector("BRANCH" -> orderId / "BRANCH")),
      OrderJoined(OrderOutcome.Failed(Some("Order:🟩|BRANCH Failed"))),
      OrderFailed(Position(0)),
      OrderCancelled,
      OrderDeleted))

    assert(controller.eventWatch.eventsByKey[OrderEvent](orderId / "BRANCH") == Seq(
      OrderLocksAcquired(List(LockDemand(lockPath))),
      OrderOutcomeAdded(OrderOutcome.failed),
      OrderLocksReleased(List(lockPath)),
      OrderFailedInFork(Position(0) / "fork+BRANCH" % 0)))

    assert(controllerState.keyTo(LockState)(lockPath) ==
      LockState(
        Lock(lockPath, itemRevision = Some(ItemRevision(0))),
        Available,
        Queue.empty))

  "Forked order with same lock" in:
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
    controller.api.addOrder(FreshOrder(orderId, workflow.path, deleteWhenTerminated = true))
      .await(99.s).orThrow

    controller.eventWatch.await[OrderFailed](_.key == orderId / "BRANCH")
    controller.api.executeCommand(CancelOrders(Seq(orderId / "BRANCH", orderId)))
      .await(99.s).orThrow
    assert(controller.eventWatch.eventsByKey[OrderEvent](orderId) == Seq(
      OrderAdded(workflow.id, deleteWhenTerminated = true),
      OrderStarted,
      OrderLocksAcquired(List(LockDemand(lockPath))),
      OrderForked(Vector("BRANCH" -> orderId / "BRANCH")),
      OrderCancellationMarked(),
      OrderJoined(OrderOutcome.Failed(Some("Order:🟪|BRANCH has been cancelled"))),
      OrderLocksReleased(List(lockPath)),
      OrderFailed(Position(0)),
      OrderCancelled,
      OrderDeleted))

    assert(controller.eventWatch.eventsByKey[OrderEvent](orderId / "BRANCH") == Seq(
      OrderOutcomeAdded(OrderOutcome.Disrupted(Problem(
        "Lock:LOCK has already been acquired by parent Order:🟪"))),
      OrderFailed(Position(0) / "lock" % 0 / "fork+BRANCH" % 0),
      OrderCancelled))

    assert(controllerState.keyTo(LockState)(lockPath) ==
      LockState(
        Lock(lockPath, itemRevision = Some(ItemRevision(0))),
        Available,
        Queue.empty))

  "Finish instruction" in:
    val workflow = updateItem(Workflow(
      WorkflowPath("FINISH"),
      Seq(
        EmptyJob.execute(agentPath),  // Complicate with a Lock at non-first position
        LockInstruction.single(
          lockPath,
          count = None,
          lockedWorkflow = Workflow.of(Finish())))))

    val orderId = OrderId("🟫")
    controller.api.addOrder(FreshOrder(orderId, workflow.path, deleteWhenTerminated = true))
      .await(99.s).orThrow
    controller.eventWatch.await[OrderFinished](_.key == orderId)

    assert(controller.eventWatch.eventsByKey[OrderEvent](orderId) == Seq(
      OrderAdded(workflow.id, deleteWhenTerminated = true),

      OrderAttachable(agentPath),
      OrderAttached(agentPath),
      OrderStarted,
      OrderProcessingStarted(subagentId),
      OrderProcessed(OrderOutcome.succeeded),
      OrderMoved(Position(1)),
      OrderDetachable,
      OrderDetached,

      OrderLocksAcquired(List(LockDemand(lockPath))),
      OrderLocksReleased(List(lockPath)),
      OrderFinished(),
      OrderDeleted))

    assert(controllerState.keyTo(LockState)(lockPath) ==
      LockState(
        Lock(lockPath, itemRevision = Some(ItemRevision(0))),
        Available,
        Queue.empty))

    controller.api
      .updateItems(Stream(
        AddVersion(VersionId("FINISH-REMOVED")),
        RemoveVersioned(workflow.path)))
      .await(99.s).orThrow

  "Finish instruction in Fork" in:
    val workflow = updateItem(Workflow(
      WorkflowPath("FINISH-IN-FORK"),
      Seq(
        Fork.forTest(Seq(Fork.Branch(
          ForkBranchId("BRANCH"),
          Workflow.of(
            LockInstruction.single(
              lockPath,
              count = None,
              lockedWorkflow = Workflow.of(Finish())))))))))

    val orderId = OrderId("🟣")
    controller.api.addOrder(FreshOrder(orderId, workflow.path, deleteWhenTerminated = true))
      .await(99.s).orThrow
    controller.eventWatch.await[OrderFinished](_.key == orderId)

    assert(controller.eventWatch.eventsByKey[OrderEvent](orderId / "BRANCH") == Seq(
      OrderLocksAcquired(List(LockDemand(lockPath))),
      OrderLocksReleased(List(lockPath)),
      OrderMoved(Position(0) / BranchId.fork("BRANCH") % 1)))

    assert(controllerState.keyTo(LockState)(lockPath) ==
      LockState(
        Lock(lockPath, itemRevision = Some(ItemRevision(0))),
        Available,
        Queue.empty))

    controller.api
      .updateItems(Stream(
        AddVersion(VersionId("FINISH-IN-FORK-REMOVED")),
        RemoveVersioned(workflow.path)))
      .await(99.s).orThrow

  "Acquire too much" in:
    val workflow = defineWorkflow(workflowNotation = """
      define workflow {
        lock (lock = "LOCK", count = 2) {}
      }""")
    val orderId = OrderId("🟧")
    controller.api.addOrder(FreshOrder(orderId, workflow.path, deleteWhenTerminated = true))
      .await(99.s).orThrow

    controller.eventWatch.await[OrderFailed](_.key == orderId)
    controller.api.executeCommand(CancelOrders(Seq(orderId))).await(99.s).orThrow
    assert(controller.eventWatch.eventsByKey[OrderEvent](orderId) == Seq(
      OrderAdded(workflow.id, deleteWhenTerminated = true),
      OrderStarted,
      OrderOutcomeAdded(OrderOutcome.Disrupted(Problem(
        "Cannot fulfill lock count=2 with Lock:LOCK limit=1"))),
      OrderFailed(Position(0)),
      OrderCancelled,
      OrderDeleted))

    assert(controllerState.keyTo(LockState)(lockPath) ==
      LockState(
        Lock(lockPath, itemRevision = Some(ItemRevision(0))),
        Available,
        Queue.empty))

  "Multiple locks" - {
    "All locks are available" in:
      val aWorkflow = updateItem(Workflow(
        WorkflowPath("MULTIPLE-A"),
        Seq(
          LockInstruction(
            Seq(
              LockDemand(lockPath),
              LockDemand(lock2Path)),
            lockedWorkflow = Workflow.of(
              ASemaphoreJob.execute(agentPath))))))

      val bWorkflow = updateItem(Workflow(
        WorkflowPath("MULTIPLE-B"),
        Seq(
          LockInstruction(
            Seq(
              LockDemand(lock2Path),
              LockDemand(lockPath)),
            lockedWorkflow = Workflow.of(
              EmptyJob.execute(agentPath))))))

      val aOrderId = OrderId("🔸")
      val bOrderId = OrderId("🔹")

      ASemaphoreJob.reset()
      controller.api.addOrder(FreshOrder(aOrderId, aWorkflow.path, deleteWhenTerminated = true))
        .await(99.s).orThrow
      controller.eventWatch.await[OrderStdoutWritten](_.key == aOrderId)

      controller.api.addOrder(FreshOrder(bOrderId, bWorkflow.path, deleteWhenTerminated = true))
        .await(99.s).orThrow
      controller.eventWatch.await[OrderLocksQueued](_.key == bOrderId)

      assert(controllerState.keyTo(LockState)(lockPath) ==
        LockState(
          Lock(lockPath, itemRevision = Some(ItemRevision(0))),
          Exclusive(aOrderId),
          Queue(bOrderId)))
      assert(controllerState.keyTo(LockState)(lock2Path) ==
        LockState(
          Lock(lock2Path, itemRevision = Some(ItemRevision(0))),
          Exclusive(aOrderId),
          Queue(bOrderId)))

      ASemaphoreJob.continue()
      controller.eventWatch.await[OrderDeleted](_.key == aOrderId)
      controller.eventWatch.await[OrderDeleted](_.key == bOrderId)

      assert(controller.eventWatch.eventsByKey[OrderEvent](aOrderId) == Seq(
        OrderAdded(aWorkflow.id, deleteWhenTerminated = true),
        OrderStarted,
        OrderLocksAcquired(List(
          LockDemand(lockPath),
          LockDemand(lock2Path))),
        OrderAttachable(agentPath),
        OrderAttached(agentPath),
        OrderProcessingStarted(subagentId),
        OrderStdoutWritten("ASemaphoreJob\n"),
        OrderProcessed(OrderOutcome.succeeded),
        OrderMoved(Position(0) / "lock" % 1),
        OrderDetachable,
        OrderDetached,
        OrderLocksReleased(List(
          lockPath,
          lock2Path)),
        OrderFinished(),
        OrderDeleted))
      assert(controllerState.keyTo(LockState)(lockPath) ==
        LockState(
          Lock(lockPath, itemRevision = Some(ItemRevision(0)))))
      assert(controllerState.keyTo(LockState)(lock2Path) ==
        LockState(
          Lock(lock2Path, itemRevision = Some(ItemRevision(0)))))

      controller.api
        .updateItems(Stream(
          AddVersion(VersionId("MULTIPLE-1-DELETE")),
          RemoveVersioned(aWorkflow.path),
          RemoveVersioned(bWorkflow.path)))
        .await(99.s).orThrow

    "One of two locks is not available" in:
      // a: Lock lockPath
      // b: Lock lockPath, lock2Path
      // c: Lock lock2Path

      val aWorkflow = updateItem(Workflow(
        WorkflowPath("MULTIPLE-A"),
        Seq(
          LockInstruction(
            Seq(
              LockDemand(lockPath)),
            lockedWorkflow = Workflow.of(
              ASemaphoreJob.execute(agentPath))))))

      val bWorkflow = updateItem(Workflow(
        WorkflowPath("MULTIPLE-B"),
        Seq(
          LockInstruction(
            Seq(
              LockDemand(lockPath),
              LockDemand(lock2Path)),
            lockedWorkflow = Workflow.of(
              EmptyJob.execute(agentPath))))))

      val cWorkflow = updateItem(Workflow(
        WorkflowPath("MULTIPLE-C"),
        Seq(
          LockInstruction(
            Seq(
              LockDemand(lock2Path)),
            lockedWorkflow = Workflow.of(
              BSemaphoreJob.execute(agentPath))))))

      val aOrderId = OrderId("♣️️")
      ASemaphoreJob.reset()
      controller.api.addOrder(FreshOrder(aOrderId, aWorkflow.path, deleteWhenTerminated = true))
        .await(99.s).orThrow
      controller.eventWatch.await[OrderStdoutWritten](_.key == aOrderId)

      val bOrderId = OrderId("♦️")
      controller.api.addOrder(FreshOrder(bOrderId, bWorkflow.path, deleteWhenTerminated = true))
        .await(99.s).orThrow
      controller.eventWatch.await[OrderLocksQueued](_.key == bOrderId)

      assert(controllerState.keyTo(LockState)(lockPath) ==
        LockState(
          Lock(lockPath, itemRevision = Some(ItemRevision(0))),
          Exclusive(aOrderId),
          Queue(bOrderId)))
      assert(controllerState.keyTo(LockState)(lock2Path) ==
        LockState(
          Lock(lock2Path, itemRevision = Some(ItemRevision(0))),
          Available,
          Queue(bOrderId)))

      val cOrderId = OrderId("♠️")
      BSemaphoreJob.reset()
      controller.api.addOrder(FreshOrder(cOrderId, cWorkflow.path, deleteWhenTerminated = true))
        .await(99.s).orThrow
      controller.eventWatch.await[OrderStdoutWritten](_.key == aOrderId)

      assert(controllerState.keyTo(LockState)(lockPath) ==
        LockState(
          Lock(lockPath, itemRevision = Some(ItemRevision(0))),
          Exclusive(aOrderId),
          Queue(bOrderId)))
      assert(controllerState.keyTo(LockState)(lock2Path) ==
        LockState(
          Lock(lock2Path, itemRevision = Some(ItemRevision(0))),
          Exclusive(cOrderId),
          Queue(bOrderId)))

      ASemaphoreJob.continue()
      controller.eventWatch.await[OrderDeleted](_.key == aOrderId)
      assert(controller.eventWatch.eventsByKey[OrderEvent](aOrderId) == Seq(
        OrderAdded(aWorkflow.id, deleteWhenTerminated = true),
        OrderStarted,
        OrderLocksAcquired(List(
          LockDemand(lockPath))),
        OrderAttachable(agentPath),
        OrderAttached(agentPath),
        OrderProcessingStarted(subagentId),
        OrderStdoutWritten("ASemaphoreJob\n"),
        OrderProcessed(OrderOutcome.succeeded),
        OrderMoved(Position(0) / "lock" % 1),
        OrderDetachable,
        OrderDetached,
        OrderLocksReleased(List(
          lockPath)),
        OrderFinished(),
        OrderDeleted))

      BSemaphoreJob.continue()
      controller.eventWatch.await[OrderDeleted](_.key == cOrderId)
      controller.eventWatch.await[OrderDeleted](_.key == bOrderId)
      assert(controller.eventWatch.eventsByKey[OrderEvent](bOrderId) == Seq(
        OrderAdded(bWorkflow.id, deleteWhenTerminated = true),
        OrderStarted,
        OrderLocksQueued(List(
          LockDemand(lockPath),
          LockDemand(lock2Path))),
        OrderLocksAcquired(List(
          LockDemand(lockPath),
          LockDemand(lock2Path))),
        OrderAttachable(agentPath),
        OrderAttached(agentPath),
        OrderProcessingStarted(subagentId),
        OrderProcessed(OrderOutcome.succeeded),
        OrderMoved(Position(0) / "lock" % 1),
        OrderDetachable,
        OrderDetached,
        OrderLocksReleased(List(
          lockPath,
          lock2Path)),
        OrderFinished(),
        OrderDeleted))

      assert(controllerState.keyTo(LockState)(lockPath) ==
        LockState(
          Lock(lockPath, itemRevision = Some(ItemRevision(0)))))
      assert(controllerState.keyTo(LockState)(lock2Path) ==
        LockState(
          Lock(lock2Path, itemRevision = Some(ItemRevision(0)))))

      controller.api
        .updateItems(Stream(
          AddVersion(VersionId("MULTIPLE-2-DELETE")),
          RemoveVersioned(aWorkflow.path),
          RemoveVersioned(bWorkflow.path),
          RemoveVersioned(cWorkflow.path)))
        .await(99.s).orThrow
  }

  "Cancel while waiting for lock" in:
    val lockingOrderId = OrderId("CANCEL-WHILE-QUEUING-IN-LOCK")
    controller.api
      .addOrder(FreshOrder(lockingOrderId, promptInLockWorkflow.path, deleteWhenTerminated = true))
      .await(99.s).orThrow
    controller.eventWatch.await[OrderPrompted](_.key == lockingOrderId)

    val queueingWorkflow = updateItem(Workflow(
      WorkflowPath("CANCEL-WHILE-QUEUING-FOR-LOCKING"),
      Seq(
        LockInstruction.single(lockPath,
          count = None,
          lockedWorkflow = Workflow.empty))))
    val orderId = OrderId("CANCEL-WHILE-QUEUING-FOR-LOCK")
    controller.api.addOrder(FreshOrder(orderId, queueingWorkflow.path, deleteWhenTerminated = true))
      .await(99.s).orThrow
    controller.eventWatch.await[OrderLocksQueued](_.key == orderId)

    controller.api.executeCommand(CancelOrders(Seq(orderId))).await(99.s).orThrow
    controller.eventWatch.await[OrderCancelled](_.key == orderId)

    assert(controller.eventWatch.eventsByKey[OrderEvent](orderId) == Seq(
      OrderAdded(queueingWorkflow.id, deleteWhenTerminated = true),
      OrderStarted,
      OrderLocksQueued(List(LockDemand(lockPath))),
      OrderStateReset,
      OrderCancelled,
      OrderDeleted))

    controller.api.executeCommand(AnswerOrderPrompt(lockingOrderId)).await(99.s).orThrow
    controller.eventWatch.await[OrderDeleted](_.key == lockingOrderId)
    assert(controller.eventWatch.eventsByKey[OrderEvent](lockingOrderId) == Seq(
      OrderAdded(promptInLockWorkflow.id, deleteWhenTerminated = true),
      OrderStarted,
      OrderLocksAcquired(List(LockDemand(lockPath))),
      OrderPrompted(StringValue("PROMPT")),
      OrderPromptAnswered(),
      OrderMoved(Position(0) / "lock" % 1),
      OrderLocksReleased(List(lockPath)),
      OrderFinished(),
      OrderDeleted))

    assert(controllerState.keyTo(LockState)(lockPath) ==
      LockState(
        Lock(lockPath, itemRevision = Some(ItemRevision(0))),
        Available,
        Queue.empty))

    controller.api.updateItems(Stream(
      AddVersion(VersionId("DELETE")),
      RemoveVersioned(queueingWorkflow.path)
    )).await(99.s).orThrow

  "Cancel while using a lock" in:
    val orderId = OrderId("CANCEL-WHILE-IN-LOCK")
    controller.api
      .addOrder(FreshOrder(orderId, promptInLockWorkflow.path, deleteWhenTerminated = true))
      .await(99.s).orThrow
    controller.eventWatch.await[OrderPrompted](_.key == orderId)

    controller.api.executeCommand(CancelOrders(Seq(orderId))).await(99.s).orThrow
    assert(controller.eventWatch.eventsByKey[OrderEvent](orderId) == Seq(
      OrderAdded(promptInLockWorkflow.id, deleteWhenTerminated = true),
      OrderStarted,
      OrderLocksAcquired(List(LockDemand(lockPath))),
      OrderPrompted(StringValue("PROMPT")),
      OrderStateReset,
      OrderLocksReleased(List(lockPath)),
      OrderCancelled,
      OrderDeleted))

    assert(controllerState.keyTo(LockState)(lockPath) ==
      LockState(
        Lock(lockPath, itemRevision = Some(ItemRevision(0))),
        Available,
        Queue.empty))

  "Use Lock both exclusivly and non-exclusively" in:
    val exclusiveWorkflow = updateItem(Workflow(
      WorkflowPath("EXCLUSIVE-WORKFLOW"),
      Seq(
        LockInstruction.single(limit2LockPath,
          count = None,
          lockedWorkflow = Workflow.of(Prompt(expr("'?'")))))))
    val nonExclusiveWorkflow = updateItem(Workflow(
      WorkflowPath("NON-EXCLUSIVE-WORKFLOW"),
      Seq(
        LockInstruction.single(limit2LockPath,
          count = Some(2),
          lockedWorkflow = Workflow.of(Prompt(expr("'?'")))))))

    locally:
      // First exclusive, then non-exclusive is queued
      val aOrder = FreshOrder(OrderId("EXCLUSIVE-A"), exclusiveWorkflow.path,
        deleteWhenTerminated = true)
      controller.api.addOrder(aOrder).await(99.s).orThrow
      eventWatch.await[OrderPrompted](_.key == aOrder.id)

      val bOrder = FreshOrder(OrderId("EXCLUSIVE-B"), nonExclusiveWorkflow.path,
        deleteWhenTerminated = true)
      controller.api.addOrder(bOrder).await(99.s).orThrow
      eventWatch.await[OrderLocksQueued](_.key == bOrder.id)

      controller.api.executeCommand(AnswerOrderPrompt(aOrder.id)).await(99.s).orThrow
      eventWatch.await[OrderFinished](_.key == aOrder.id)

      eventWatch.await[OrderPrompted](_.key == bOrder.id)
      controller.api.executeCommand(AnswerOrderPrompt(bOrder.id)).await(99.s).orThrow
      eventWatch.await[OrderFinished](_.key == bOrder.id)

    locally:
      // First non-exclusive, then exclusive is queued
      val aOrder = FreshOrder(OrderId("EXCLUSIVE-AA"), exclusiveWorkflow.path,
        deleteWhenTerminated = true)
      controller.api.addOrder(aOrder).await(99.s).orThrow
      eventWatch.await[OrderPrompted](_.key == aOrder.id)

      val bOrder = FreshOrder(OrderId("EXCLUSIVE-BB"), nonExclusiveWorkflow.path,
        deleteWhenTerminated = true)
      controller.api.addOrder(bOrder).await(99.s).orThrow
      eventWatch.await[OrderLocksQueued](_.key == bOrder.id)

      controller.api.executeCommand(AnswerOrderPrompt(aOrder.id)).await(99.s).orThrow
      eventWatch.await[OrderFinished](_.key == aOrder.id)
      eventWatch.await[OrderDeleted](_.key == aOrder.id)

      eventWatch.await[OrderPrompted](_.key == bOrder.id)
      controller.api.executeCommand(AnswerOrderPrompt(bOrder.id)).await(99.s).orThrow
      eventWatch.await[OrderFinished](_.key == bOrder.id)
      eventWatch.await[OrderDeleted](_.key == bOrder.id)

    controller.api.updateItems(Stream(
      AddVersion(VersionId("REMOVE-EXCLUSIVE")),
      RemoveVersioned(exclusiveWorkflow.path),
      RemoveVersioned(nonExclusiveWorkflow.path)
    )).await(99.s).orThrow

  "JS-2015 Lock in Try/Retry with zero delay" in:
    val workflow = Workflow(
      WorkflowPath("RETRY-LOCK"),
      Seq(
        TryInstruction(
          Workflow.of(
            LockInstruction.single(lockPath,
              count = None,
              lockedWorkflow = Workflow.of(
                FailingJob.execute(agentPath)))),
          Workflow.of(
            Retry()),
            retryDelays = Some(Vector(0.s)),
            maxTries = Some(2))))

    withTemporaryItem(workflow) { workflow =>
      val orderId = OrderId("RETRY-LOCK")
      val events = controller.runOrder(
        FreshOrder(orderId, workflow.path, deleteWhenTerminated = true))
      assert(events.map(_.value) == Seq(
        OrderAdded(workflow.id, deleteWhenTerminated = true),
        OrderMoved(Position(0) / "try+0" % 0),
        OrderStarted,

        OrderLocksAcquired(List(LockDemand(lockPath))),
        OrderAttachable(agentPath),
        OrderAttached(agentPath),
        OrderProcessingStarted(subagentId),
        OrderProcessed(OrderOutcome.Failed(Some("💥FailingJob failed💥"))),
        OrderDetachable,
        OrderDetached,
        OrderLocksReleased(List(lockPath)),

        OrderCaught(Position(0) / "catch+0" % 0),
        OrderRetrying(),
        OrderMoved(Position(0) / "try+1" % 0),

        OrderLocksAcquired(List(LockDemand(lockPath))),
        OrderAttachable(agentPath),
        OrderAttached(agentPath),
        OrderProcessingStarted(subagentId),
        OrderProcessed(OrderOutcome.Failed(Some("💥FailingJob failed💥"))),
        OrderDetachable,
        OrderDetached,
        OrderLocksReleased(List(lockPath)),

        OrderFailed(Position(0) / "try+1" % 0)))

      controller.api.executeCommand(CancelOrders(Seq(orderId))).await(99.s).orThrow
      eventWatch.await[OrderDeleted](_.key == orderId)
    }

  "JS-2015 Lock in Try/Retry with delay" in:
    val workflow = Workflow(
      WorkflowPath("RETRY-LOCK"),
      Seq(
        TryInstruction(
          Workflow.of(
            LockInstruction.single(lockPath,
              count = None,
              lockedWorkflow = Workflow.of(
                FailingJob.execute(agentPath)))),
          Workflow.of(
            Retry()),
            retryDelays = Some(Vector(1.ms)),
            maxTries = Some(2))))

    withTemporaryItem(workflow, awaitDeletion = true) { workflow =>
      val orderId = OrderId("RETRY-LOCK")
      val events = controller.runOrder(
        FreshOrder(orderId, workflow.path, deleteWhenTerminated = true))
      assert(events.map(_.value)
        .map {
          case o @ OrderRetrying(Some(_), None) => o.copy(delayedUntil = Some(Timestamp.Epoch))
          case o => o
        } == Seq(
        OrderAdded(workflow.id, deleteWhenTerminated = true),
        OrderMoved(Position(0) / "try+0" % 0),
        OrderStarted,

        OrderLocksAcquired(List(LockDemand(lockPath))),
        OrderAttachable(agentPath),
        OrderAttached(agentPath),
        OrderProcessingStarted(subagentId),
        OrderProcessed(OrderOutcome.Failed(Some("💥FailingJob failed💥"))),
        OrderDetachable,
        OrderDetached,
        OrderLocksReleased(List(lockPath)),

        OrderCaught(Position(0) / "catch+0" % 0),
        OrderRetrying(Some(Timestamp.Epoch)),
        OrderAwoke,
        OrderMoved(Position(0) / "try+1" % 0),

        OrderLocksAcquired(List(LockDemand(lockPath))),
        OrderAttachable(agentPath),
        OrderAttached(agentPath),
        OrderProcessingStarted(subagentId),
        OrderProcessed(OrderOutcome.Failed(Some("💥FailingJob failed💥"))),
        OrderDetachable,
        OrderDetached,
        OrderLocksReleased(List(lockPath)),

        OrderFailed(Position(0) / "try+1" % 0)))

      controller.api.executeCommand(CancelOrders(Seq(orderId))).await(99.s).orThrow
      eventWatch.await[OrderDeleted](_.key == orderId)
    }

  "OrderLockReleased wakes waiting orders (JS-2157)" - {
    "After OrderLocksReleased continue waiting Order" in:
      val workflow = Workflow(
        WorkflowPath("LOCK-CONTINUE-QUEUED"),
        Seq:
          LockInstruction.single(lockPath,
            count = None,
            lockedWorkflow = Workflow.of(
              Prompt(expr("'PROMPT'")))))

      withTemporaryItem(workflow, awaitDeletion = true): workflow =>
        val eventId = eventWatch.lastAddedEventId

        val aOrderId = OrderId("LOCK-CONTINUE-QUEUED-A")
        controller.api
          .addOrder:
            FreshOrder(aOrderId, workflow.path, deleteWhenTerminated = true)
          .await(99.s).orThrow

        eventWatch.awaitNext[OrderLocksAcquired](_.key == aOrderId)

        val bOrderId = OrderId("LOCK-CONTINUE-QUEUED-B")
        controller.api
          .addOrder:
            FreshOrder(bOrderId, workflow.path, deleteWhenTerminated = true)
          .await(99.s).orThrow

        eventWatch.awaitNext[OrderLocksQueued](_.key == bOrderId)

        controller.api.executeCommand(AnswerOrderPrompt(aOrderId)).await(99.s).orThrow
        eventWatch.awaitNext[OrderLocksReleased](_.key == aOrderId)
        eventWatch.await[OrderTerminated](_.key == aOrderId, after = eventId)

        locally:
          val keyedEvents = eventWatch.keyedEvents[OrderEvent](_.key == aOrderId, after = eventId)
          assert(keyedEvents.map(_.event) == Seq(
            OrderAdded(workflow.id, deleteWhenTerminated = true),
            OrderStarted,
            OrderLocksAcquired(List(LockDemand(lockPath))),
            OrderPrompted(StringValue("PROMPT")),
            OrderPromptAnswered(),
            OrderMoved(Position(0) / "lock" % 1),
            OrderLocksReleased(List(lockPath)),
            OrderFinished(),
            OrderDeleted))

        eventWatch.awaitNext[OrderLocksAcquired](_.key == bOrderId)
        controller.api.executeCommand(AnswerOrderPrompt(bOrderId)).await(99.s).orThrow
        eventWatch.awaitNext[OrderTerminated](_.key == bOrderId)

        locally:
          val keyedEvents = eventWatch.keyedEvents[OrderEvent](_.key == bOrderId, after = eventId)
          assert(keyedEvents.map(_.event) == Seq(
            OrderAdded(workflow.id, deleteWhenTerminated = true),
            OrderStarted,
            OrderLocksQueued(List(LockDemand(lockPath))),
            OrderLocksAcquired(List(LockDemand(lockPath))),
            OrderPrompted(StringValue("PROMPT")),
            OrderPromptAnswered(),
            OrderMoved(Position(0) / "lock" % 1),
            OrderLocksReleased(List(lockPath)),
            OrderFinished(),
            OrderDeleted))

    "After OrderLocksReleased due to cancellation in Prompt continue waiting Order" in :
      val workflow = Workflow(
        WorkflowPath("LOCK-CONTINUE-QUEUED"),
        Seq:
          LockInstruction.single(lockPath,
            count = None,
            lockedWorkflow = Workflow.of(
              Prompt(expr("'PROMPT'")),
              Fail())))

      withTemporaryItem(workflow, awaitDeletion = true): workflow =>
        val eventId = eventWatch.lastAddedEventId

        val aOrderId = OrderId("LOCK-CONTINUE-QUEUED-A")
        controller.api
          .addOrder:
            FreshOrder(aOrderId, workflow.path, deleteWhenTerminated = true)
          .await(99.s).orThrow

        eventWatch.awaitNext[OrderLocksAcquired](_.key == aOrderId)

        val bOrderId = OrderId("LOCK-CONTINUE-QUEUED-B")
        controller.api
          .addOrder:
            FreshOrder(bOrderId, workflow.path, deleteWhenTerminated = true)
          .await(99.s).orThrow

        eventWatch.awaitNext[OrderLocksQueued](_.key == bOrderId)

        controller.api.executeCommand(CancelOrders(Seq(aOrderId))).await(99.s).orThrow
        eventWatch.awaitNext[OrderLocksReleased](_.key == aOrderId)

        locally:
          val keyedEvents = eventWatch.keyedEvents[OrderEvent](_.key == aOrderId, after = eventId)
          assert(keyedEvents.map(_.event) == Seq(
            OrderAdded(workflow.id, deleteWhenTerminated = true),
            OrderStarted,
            OrderLocksAcquired(List(LockDemand(lockPath))),
            OrderPrompted(StringValue("PROMPT")),
            OrderStateReset,
            OrderLocksReleased(List(lockPath)),
            OrderCancelled,
            OrderDeleted))

        eventWatch.awaitNext[OrderLocksAcquired](_.key == bOrderId)
        controller.api.executeCommand(CancelOrders(Seq(bOrderId))).await(99.s).orThrow

        eventWatch.await[OrderTerminated](_.key == aOrderId, after = eventId)
        eventWatch.await[OrderTerminated](_.key == bOrderId, after = eventId)

        locally:
          val keyedEvents = eventWatch.keyedEvents[OrderEvent](_.key == bOrderId, after = eventId)
          assert(keyedEvents.map(_.event) == Seq(
            OrderAdded(workflow.id, deleteWhenTerminated = true),
            OrderStarted,
            OrderLocksQueued(List(LockDemand(lockPath))),
            OrderLocksAcquired(List(LockDemand(lockPath))),
            OrderPrompted(StringValue("PROMPT")),
            OrderStateReset,
            OrderLocksReleased(List(lockPath)),
            OrderCancelled,
            OrderDeleted))

    "After OrderLocksReleased in stopOnFailure-block continue waiting Order" in:
      val workflow = Workflow(
        WorkflowPath("STOP-ON-FAILURE-LOCK"),
        Seq:
          Options(stopOnFailure = true):
            LockInstruction.single(lockPath,
              count = None,
              lockedWorkflow = Workflow.of(
                Fail())))

      withTemporaryItem(workflow, awaitDeletion = true): workflow =>
        val eventId = eventWatch.lastAddedEventId

        val aOrderId = OrderId("STOP-ON-FAILURE-LOCK-A")
        controller.api
          .addOrder:
            FreshOrder(aOrderId, workflow.path, deleteWhenTerminated = true)
          .await(99.s).orThrow

        eventWatch.awaitNext[OrderLocksAcquired](_.key == aOrderId)

        val bOrderId = OrderId("STOP-ON-FAILURE-LOCK-B")
        controller.api
          .addOrder:
            FreshOrder(bOrderId, workflow.path, deleteWhenTerminated = true)
          .await(99.s).orThrow

        eventWatch.awaitNext[OrderLocksQueued](_.key == bOrderId)

        controller.api.executeCommand(CancelOrders(Seq(aOrderId))).await(99.s).orThrow
        eventWatch.awaitNext[OrderTerminated](_.key == aOrderId)

        locally:
          val keyedEvents = eventWatch.keyedEvents[OrderEvent](_.key == aOrderId, after = eventId)
          assert(keyedEvents.map(_.event) == Seq(
            OrderAdded(workflow.id, deleteWhenTerminated = true),
            OrderMoved(Position(0) / "options" % 0),
            OrderStarted,
            OrderLocksAcquired(List(LockDemand(lockPath))),
            OrderOutcomeAdded(OrderOutcome.failed),
            OrderStopped,
            OrderLocksReleased(List(lockPath)),
            OrderCancelled,
            OrderDeleted))

        eventWatch.awaitNext[OrderLocksAcquired](_.key == bOrderId)
        eventWatch.awaitNext[OrderStopped](_.key == bOrderId)
        controller.api.executeCommand(CancelOrders(Seq(bOrderId))).await(99.s).orThrow
        eventWatch.awaitNext[OrderTerminated](_.key == bOrderId)

        locally:
          val keyedEvents = eventWatch.keyedEvents[OrderEvent](_.key == bOrderId, after = eventId)
          assert(keyedEvents.map(_.event) == Seq(
            OrderAdded(workflow.id, deleteWhenTerminated = true),
            OrderMoved(Position(0) / "options" % 0),
            OrderStarted,
            OrderLocksQueued(List(LockDemand(lockPath))),
            OrderLocksAcquired(List(LockDemand(lockPath))),
            OrderOutcomeAdded(OrderOutcome.failed),
            OrderStopped,
            OrderLocksReleased(List(lockPath)),
            OrderCancelled,
            OrderDeleted))
  }

  "TransferOrders while waiting for Lock" in:
    val eventId = eventWatch.lastAddedEventId

    val lock1 = Lock(LockPath("TRANSFER-ORDERS-1"))
    val lock2 = Lock(LockPath("TRANSFER-ORDERS-2"))

    val aWorkflow = Workflow.of(
      WorkflowPath("A-TRANSFER-ORDERS"),
      LockInstruction(
        Seq(LockDemand(lock1.path), LockDemand(lock2.path)),
        lockedWorkflow = Workflow.of(Prompt(expr("'PROMPT'")))))

    val bWorkflow1 = Workflow(
      WorkflowPath("B-TRANSFER-ORDERS"),
      Seq:
        LockInstruction(
          Seq(LockDemand(lock1.path)),
          lockedWorkflow = Workflow.empty))

    withItems((lock1, lock2, aWorkflow, bWorkflow1)): (lock1, lock2, workflow, bWorkflow1) =>
      val aOrderId = OrderId("TRANSFER-ORDERS-A")
      controller.api.addOrder(FreshOrder(aOrderId, workflow.path, deleteWhenTerminated = true))
        .await(99.s).orThrow
      eventWatch.awaitNext[OrderPrompted](_.key == aOrderId)

      val bOrderId = OrderId("TRANSFER-ORDERS-B")
      controller.api.addOrder(FreshOrder(bOrderId, bWorkflow1.path, deleteWhenTerminated = true))
        .await(99.s).orThrow
      eventWatch.awaitNext[OrderLocksQueued](_.key == bOrderId)

      val bWorkflow2 = updateItem(Workflow(
        bWorkflow1.path,
        Seq:
          LockInstruction(
            Seq(LockDemand(lock2.path)),
            lockedWorkflow = Workflow.empty)))

      execCmd(TransferOrders(bWorkflow1.id))

      execCmd(AnswerOrderPrompt(aOrderId))
      eventWatch.await[OrderTerminated](_.key == aOrderId, after = eventId)
      eventWatch.await[OrderTerminated](_.key == bOrderId, after = eventId)

      assert(eventWatch.eventsByKey[OrderEvent](bOrderId) == Seq(
        OrderAdded(bWorkflow1.id, deleteWhenTerminated = true),
        OrderStarted,
        OrderLocksQueued(List(LockDemand(lock1.path))),

        OrderStateReset,
        OrderTransferred(bWorkflow2.id /: Position(0)),
        OrderLocksQueued(List(LockDemand(lock2.path))),

        OrderLocksAcquired(List(LockDemand(lock2.path))),
        OrderLocksReleased(Seq(lock2.path)),
        OrderFinished(),
        OrderDeleted))

      assert(controllerState.keyTo(LockState)(lock1.path) == LockState(lock1, acquired = Available, queue = Queue.empty))
      assert(controllerState.keyTo(LockState)(lock2.path) == LockState(lock2, acquired = Available, queue = Queue.empty))

  "Lock is not deletable while in use by a Workflow" in:
    val workflow = defineWorkflow(workflowNotation = """
      define workflow {
        lock (lock = "LOCK") {}
      }""")
    val v = VersionId("DELETE-BUT-ORDER")
    assert(controller.api.updateItems(Stream(
      AddVersion(v),
      DeleteSimple(lockPath)
    )).await(99.s) == Left(Problem.Combined(Set(
      ItemIsStillReferencedProblem(lockPath, promptInLockWorkflow.id),
      ItemIsStillReferencedProblem(lockPath, workflow.id)))))

  "Lock is not deletable while in use by a deleted Workflow with a still running order" in:
    val workflow = defineWorkflow(workflowNotation = """
      define workflow {
        lock (lock = "LOCK") {
          prompt "LOCKED";
        }
      }""")
    val v = VersionId("DELETE-WHILE-ORDER")
    val orderId = OrderId("DELETING")
    controller.api.addOrder(FreshOrder(orderId, workflow.path, deleteWhenTerminated = true))
      .await(99.s).orThrow
    controller.eventWatch.await[OrderPrompted](_.key == orderId)

    def removeWorkflowAndLock() = controller.api
      .updateItems(Stream(
        DeleteSimple(lockPath),
        AddVersion(v),
        RemoveVersioned(workflow.path),
        RemoveVersioned(promptInLockWorkflow.path)))
      .await(99.s)
    // Lock cannot be deleted due to orders in the deleted (but still versioned) Workflow
    assert(removeWorkflowAndLock() == Left:
      ItemIsStillReferencedProblem(lockPath, workflow.id, "with Order:DELETING"))

    controller.api.executeCommand(AnswerOrderPrompt(orderId)).await(99.s).orThrow
    controller.eventWatch.await[OrderDeleted](_.key == orderId)
    removeWorkflowAndLock().orThrow

  "Remove Workflow and Lock" in:
    val eventId = controller.eventWatch.lastAddedEventId
    val previousControllerState = controllerState
    val v = VersionId("DELETE")
    controller.api.updateItems(Stream(
      DeleteSimple(lockPath),
      DeleteSimple(lock2Path),
      DeleteSimple(limit2LockPath),
      AddVersion(v),
      RemoveVersioned(workflowPath),
      RemoveVersioned(workflow1Path),
      RemoveVersioned(workflow2Path)
    )).await(99.s).orThrow
    for workflowId <- previousControllerState.repo.itemIdsFor(WorkflowPath) do
      controller.eventWatch.await[ItemDeleted](_.event.key == workflowId, after = eventId)
    previousControllerState.itemToAgentToAttachedState
      .foreach:
        case (WorkflowId.as(id), _) =>
          controller.eventWatch.await[ItemDetached](_.event.key == id, after = eventId)
        case _ =>
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
      agentPath -> Map(
        agentPath -> Attached(Some(ItemRevision(0)))),
      bAgentPath -> Map(
        bAgentPath -> Attached(Some(ItemRevision(0)))),
      SubagentId("AGENT-0") -> Map(
        AgentPath("AGENT") -> Attached(Some(ItemRevision(0)))),
      SubagentId("B-AGENT-0") -> Map(
        AgentPath("B-AGENT") -> Attached(Some(ItemRevision(0))))))

  private def defineWorkflow(workflowNotation: String): Workflow =
    defineWorkflow(workflowPath, workflowNotation)

  private def defineWorkflow(workflowPath: WorkflowPath, workflowNotation: String): Workflow =
    val workflow = WorkflowParser.parse(workflowPath, workflowNotation).orThrow
    updateItem(workflow)


object LockTest:

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
      LockInstruction.single(lockPath,
        count = None,
        lockedWorkflow = Workflow.of(
          Prompt(StringConstant("PROMPT"))))))

  final class ASemaphoreJob extends SemaphoreJob(ASemaphoreJob)
  object ASemaphoreJob extends SemaphoreJob.Companion[ASemaphoreJob]

  final class BSemaphoreJob extends SemaphoreJob(BSemaphoreJob)
  object BSemaphoreJob extends SemaphoreJob.Companion[BSemaphoreJob]
