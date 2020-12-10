package js7.data.execution.workflow.instructions

import js7.base.utils.ScalaUtils.syntax._
import js7.data.agent.AgentName
import js7.data.execution.workflow.context.OrderContext
import js7.data.execution.workflow.instructions.LockExecutorTest._
import js7.data.job.ExecutablePath
import js7.data.lock.{Acquired, Lock, LockId, LockState}
import js7.data.order.OrderEvent.{OrderLockAcquired, OrderLockQueued, OrderLockReleased}
import js7.data.order.{Order, OrderId}
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.instructions.{Execute, LockInstruction}
import js7.data.workflow.position.{BranchId, Position}
import js7.data.workflow.{Workflow, WorkflowId, WorkflowPath}
import org.scalatest.freespec.AnyFreeSpec

final class LockExecutorTest extends AnyFreeSpec {

  private lazy val context = new OrderContext {
    def idToOrder = Map(freeLockOrder.id -> freeLockOrder, freeLockedOrder.id -> freeLockedOrder, occupiedLockOrder.id -> occupiedLockOrder).checked
    def childOrderEnded(order: Order[Order.State]) = throw new NotImplementedError
    def idToWorkflow(id: WorkflowId) = Map(workflow.id -> workflow).checked(id)
    val nameToLockState = Map(
      freeLockId -> LockState(Lock(freeLockId)),
      occupiedLockId -> LockState(Lock(occupiedLockId), Acquired.Exclusive(OrderId("OCCUPANT"))),
    ).checked
  }

  "Lock acquired" in {
    assert(InstructionExecutor.toEvents(workflow.instruction(freeLockOrder.position), freeLockOrder, context) ==
      Right(Seq(freeLockOrder.id <-: OrderLockAcquired(freeLockId))))
  }

  "Lock released" in {
    assert(InstructionExecutor.toEvents(workflow.instruction(freeLockedOrder.position), freeLockedOrder, context) ==
      Right(Seq(freeLockOrder.id <-: OrderLockReleased(freeLockId))))
  }

  "Lock can not acquired" in {
    assert(InstructionExecutor.toEvents(workflow.instruction(occupiedLockOrder.position), occupiedLockOrder, context) ==
      Right(Seq(occupiedLockOrder.id <-: OrderLockQueued(occupiedLockId))))
  }

  "Lock released and waiting order continues" in {
    assert(InstructionExecutor.toEvents(workflow.instruction(freeLockedOrder.position), freeLockedOrder, context) ==
      Right(Seq(freeLockOrder.id <-: OrderLockReleased(freeLockId))))
  }
}

object LockExecutorTest {
  private val freeLockId = LockId("FREE-LOCK")
  private val occupiedLockId = LockId("OCCUPIED-LOCK")
  private val exclusiveLockId = LockId("EXCLUSIVE-LOCK")
  private val execute = Execute(WorkflowJob(AgentName("AGENT"), ExecutablePath("JOB")))

  private val workflow = Workflow.of(WorkflowPath("/WORKFLOW") ~ "VERSION",
    LockInstruction(freeLockId, exclusive = false, Workflow.of(execute)),
    LockInstruction(occupiedLockId, exclusive = false, Workflow.of(execute)),
    LockInstruction(exclusiveLockId, exclusive = false, Workflow.of(execute)))

  private val freeLockOrder = Order(OrderId("ORDER-A"), workflow.id /: Position(0), Order.Ready)
  private val freeLockedOrder = Order(OrderId("ORDER-A"), workflow.id /: (Position(0) / BranchId.Lock % 1), Order.Ready)

  private val occupiedLockOrder = Order(OrderId("ORDER-B"), workflow.id /: Position(1), Order.Ready)
}
