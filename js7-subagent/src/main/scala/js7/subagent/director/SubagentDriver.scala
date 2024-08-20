package js7.subagent.director

import cats.effect.{Deferred, FiberIO, IO}
import cats.syntax.all.*
import js7.base.crypt.Signed
import js7.base.io.process.ProcessSignal
import js7.base.monixutils.AsyncMap
import js7.base.problem.Checked
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.delegate.DelegateCouplingState.Coupled
import js7.data.item.SignableItem
import js7.data.job.{JobConf, JobKey, JobResource}
import js7.data.order.OrderEvent.OrderProcessed
import js7.data.order.{Order, OrderId}
import js7.data.subagent.{SubagentDirectorState, SubagentId, SubagentItem}
import js7.data.value.expression.Expression
import js7.data.workflow.Workflow
import js7.data.workflow.instructions.Execute
import js7.data.workflow.position.WorkflowPosition
import js7.journal.state.Journal

trait SubagentDriver:

  def subagentItem: SubagentItem

  protected def isHeartbeating: Boolean

  protected def isStopping: Boolean

  protected def isShuttingDown: Boolean

  def startObserving: IO[Unit]

  protected val journal: Journal[? <: SubagentDirectorState[?]]

  def startOrderProcessing(order: Order[Order.Processing]): IO[Checked[FiberIO[OrderProcessed]]]

  def recoverOrderProcessing(order: Order[Order.Processing]): IO[Checked[FiberIO[OrderProcessed]]]

  def killProcess(orderId: OrderId, signal: ProcessSignal): IO[Unit]

  def tryShutdown: IO[Unit]

  def stopJobs(jobKeys: Iterable[JobKey], signal: ProcessSignal): IO[Unit]

  def terminate: IO[Unit]

  protected def api: SubagentApi

  protected final val orderToDeferred =
    AsyncMap.stoppable[OrderId, Deferred[IO, OrderProcessed]]()

  final def subagentId: SubagentId =
    subagentItem.id

  final def isCoupled: Boolean =
    !isStopping &&
      !isShuttingDown &&
      isHeartbeating &&
      journal.unsafeCurrentState()
        .idToSubagentItemState.get(subagentId)
        .exists(s => s.couplingState == Coupled
          /*Due to isHeartbeating we can ignore s.problem to allow SubagentCoupled event.*/)

  protected final def orderToExecuteDefaultArguments(order: Order[Order.Processing])
  : IO[Checked[Map[String, Expression]]] =
    journal.state.map(_
      .idToWorkflow
      .checked(order.workflowId)
      .map(_.instruction(order.position))
      .map:
        case o: Execute => o.defaultArguments
        case _ => Map.empty)

  // TODO Emit one batch for all recovered orders!
  final def emitOrderProcessLost(order: Order[Order.Processing])
  : IO[Checked[OrderProcessed]] =
    journal
      .persistKeyedEvent(order.id <-: OrderProcessed.processLostDueToRestart)
      .map(_.map(_._1.value.event))

  protected final def signableItemsForOrderProcessing(workflowPosition: WorkflowPosition)
  : IO[Checked[Seq[Signed[SignableItem]]]] =
    for s <- journal.state yield
      for
        signedWorkflow <- s.keyToSigned(Workflow).checked(workflowPosition.workflowId)
        workflow = signedWorkflow.value
        jobKey <- workflow.positionToJobKey(workflowPosition.position)
        job <- workflow.keyToJob.checked(jobKey)
        jobResourcePaths = JobConf.jobResourcePathsFor(job, workflow)
        signedJobResources <- jobResourcePaths.traverse(s.keyToSigned(JobResource).checked)
      yield
        signedJobResources :+ signedWorkflow
