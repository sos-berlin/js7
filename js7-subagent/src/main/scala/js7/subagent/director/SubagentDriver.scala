package js7.subagent.director

import cats.effect.{Deferred, FiberIO, IO}
import cats.syntax.all.*
import js7.base.crypt.Signed
import js7.base.io.process.ProcessSignal
import js7.base.log.Logger
import js7.base.monixutils.AsyncMap
import js7.base.problem.Checked
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.StandardMapView
import js7.data.delegate.DelegateCouplingState.Coupled
import js7.data.item.SignableItem
import js7.data.job.{JobConf, JobKey, JobResource}
import js7.data.order.OrderEvent.OrderProcessed
import js7.data.order.{Order, OrderId}
import js7.data.subagent.Problems.ProcessLostDueToRestartProblem
import js7.data.subagent.{SubagentDirectorState, SubagentId, SubagentItem}
import js7.data.value.expression.{Expression, Scope}
import js7.data.value.{NumberValue, Value}
import js7.data.workflow.Workflow
import js7.data.workflow.instructions.Execute
import js7.data.workflow.position.WorkflowPosition
import js7.journal.Journal

trait SubagentDriver:

  // Change of disabled does not change this subagentItem.
  // Then, it differs from the original SubagentItem
  def subagentItem: SubagentItem

  protected def isHeartbeating: Boolean

  protected def isStopping: Boolean

  protected def isShuttingDown: Boolean

  def startObserving: IO[Unit]

  protected val journal: Journal[? <: SubagentDirectorState[?]]

  def startOrderProcessing(order: Order[Order.Processing]): IO[Checked[FiberIO[OrderProcessed]]]

  def recoverOrderProcessing(order: Order[Order.Processing]): IO[Checked[FiberIO[OrderProcessed]]]

  def killProcess(orderId: OrderId, signal: ProcessSignal): IO[Unit]

  def serverMeteringScope(): Option[Scope]

  def tryShutdownForRemoval: IO[Unit]

  def stopJobs(jobKeys: Iterable[JobKey], signal: ProcessSignal): IO[Unit]

  def terminate: IO[Unit]

  protected def api: SubagentApi

  private val logger = Logger.withPrefix[this.type](subagentItem.pathRev.toString)
  protected final val orderToDeferred =
    AsyncMap.stoppable[OrderId, Deferred[IO, OrderProcessed]]()

  val subagentProcessCountScope: Scope =
    val Key = "js7SubagentProcessCount"
    new Scope:
      override val nameToCheckedValue =
        new StandardMapView[String, Checked[Value]]:
          override val keySet = Set(Key)

          override def get(key: String) =
            key match
              case Key => Some(Right(NumberValue(processCount)))
              case _ => None

  final def subagentId: SubagentId =
    subagentItem.id

  private def processCount: Int =
    orderToDeferred.size

  final def isCoupled: Boolean =
    !isStopping &&
      !isShuttingDown &&
      isHeartbeating &&
      journal.unsafeAggregate()
        .idToSubagentItemState.get(subagentId)
        .exists(s => s.couplingState == Coupled
          /*Due to isHeartbeating we can ignore s.problem to allow SubagentCoupled event.*/)

  protected final def orderToExecuteDefaultArguments(order: Order[Order.Processing])
  : IO[Checked[Map[String, Expression]]] =
    journal.aggregate.map(_
      .idToWorkflow
      .checked(order.workflowId)
      .map(_.instruction(order.position))
      .map:
        case o: Execute => o.defaultArguments
        case _ => Map.empty)

  // TODO Emit one batch for all recovered orders!
  final def emitOrderProcessLostAfterRestart(order: Order[Order.Processing])
  : IO[Checked[OrderProcessed]] =
    journal.persist: aggregate =>
      Right(Seq:
        order.id <-: aggregate.orderProcessLostIfRestartable(order, ProcessLostDueToRestartProblem))
    .map(_.flatMap(_
      .checkedSingle.map(_._1.value.event)))

  protected final def signableItemsForOrderProcessing(workflowPosition: WorkflowPosition)
  : IO[Checked[Seq[Signed[SignableItem]]]] =
    for s <- journal.aggregate yield
      for
        signedWorkflow <- s.keyToSigned(Workflow).checked(workflowPosition.workflowId)
        workflow = signedWorkflow.value
        jobKey <- workflow.positionToJobKey(workflowPosition.position)
        job <- workflow.keyToJob.checked(jobKey)
        jobResourcePaths = JobConf.jobResourcePathsFor(job, workflow)
        signedJobResources <- jobResourcePaths.traverse(s.keyToSigned(JobResource).checked)
      yield
        signedJobResources :+ signedWorkflow

  protected final def onOrderProcessed(orderId: OrderId, orderProcessed: OrderProcessed)
  : IO[Option[IO[Unit]]] =
    orderToDeferred.remove(orderId).map:
      case None =>
        logger.error(s"Unknown Order for event: ${orderId <-: orderProcessed}")
        None

      case Some(processing) =>
        Some(processing.complete(orderProcessed).void)
