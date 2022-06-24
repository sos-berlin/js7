package js7.data.state

import cats.syntax.semigroup._
import js7.base.problem.Checked._
import js7.base.problem.{Checked, Problem}
import js7.base.time.Timestamp
import js7.base.utils.ScalaUtils.syntax._
import js7.data.board.{BoardPath, BoardState}
import js7.data.controller.ControllerId
import js7.data.event.ItemContainer
import js7.data.item.{UnsignedSimpleItem, UnsignedSimpleItemPath, UnsignedSimpleItemState}
import js7.data.job.{JobKey, JobResource}
import js7.data.order.Order.{FailedInFork, Processing}
import js7.data.order.OrderEvent.OrderNoticesExpected
import js7.data.order.{Order, OrderId}
import js7.data.value.expression.Scope
import js7.data.value.expression.scopes.{JobResourceScope, NowScope, OrderScopes}
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.instructions.{BoardInstruction, End}
import js7.data.workflow.position.{Label, WorkflowPosition}
import js7.data.workflow.{Instruction, Workflow, WorkflowId, WorkflowPath, WorkflowPathControl, WorkflowPathControlState}
import scala.collection.MapView
import scala.reflect.ClassTag

/** Common interface for ControllerState and AgentState (but not SubagentState). */
trait StateView extends ItemContainer
{
  def isAgent: Boolean

  def controllerId: ControllerId

  def idToOrder: PartialFunction[OrderId, Order[Order.State]]

  def orders: Iterable[Order[Order.State]]

  // SLOW !!!
  def jobToOrderCount(jobKey: JobKey): Int =
    idToWorkflow
      .get(jobKey.workflowId)
      .fold(0)(workflow =>
        orders.view
          .count(order =>
            order.state.isInstanceOf[Processing] &&
              order.workflowId == jobKey.workflowId &&
              workflow.positionToJobKey(order.position).contains(jobKey)))

  def idToWorkflow: PartialFunction[WorkflowId, Workflow]

  def workflowPathToId(workflowPath: WorkflowPath): Checked[WorkflowId]

  def pathToWorkflowPathControlState: MapView[WorkflowPath, WorkflowPathControlState]

  final def pathToWorkflowPathControl: MapView[WorkflowPath, WorkflowPathControl] =
    pathToWorkflowPathControlState.view.mapValues(_.workflowPathControl)

  def pathToItemState: MapView[UnsignedSimpleItemPath, UnsignedSimpleItemState]

  final def pathTo[A <: UnsignedSimpleItemState](A: UnsignedSimpleItemState.Companion[A])
  : MapView[A.Path, A] =
    pathToItemState
      .filter { case (_, v) => v.companion eq A }
      .asInstanceOf[MapView[A.Path, A]]

  final def pathTo[A <: UnsignedSimpleItem](A: UnsignedSimpleItem.Companion[A])
  : MapView[A.Path, A] =
    pathToItemState
      .filter { case (_, v) => v.item.companion eq A }
      .mapValues(_.item)
      .asInstanceOf[MapView[A.Path, A]]

  //final def pathToCheckedItem[P <: UnsignedSimpleItemPath](path: P)
  //  (implicit P: UnsignedSimpleItemPath.Companion[P])
  //: Checked[P.Item] =
  //  pathToItemState.checked(path).asInstanceOf[Checked[P.Item]]

  def availableNotices(expectedSeq: Iterable[OrderNoticesExpected.Expected]): Set[BoardPath] =
    expectedSeq
      .collect {
        case x if pathTo(BoardState).get(x.boardPath).exists(_ containsNotice x.noticeId) =>
          x.boardPath
      }
      .toSet

  // COMPATIBLE with v2.3
  final def workflowPositionToBoardState(workflowPosition: WorkflowPosition): Checked[BoardState] =
    for {
      boardPath <- workflowPositionToBoardPath(workflowPosition)
      boardState <- pathTo(BoardState).checked(boardPath)
    } yield boardState

  // COMPATIBLE with v2.3
  final def workflowPositionToBoardPath(workflowPosition: WorkflowPosition): Checked[BoardPath] =
    instruction_[BoardInstruction](workflowPosition)
      .flatMap(_.referencedBoardPaths.toSeq match {
        case Seq(boardPath) => Right(boardPath)
        case _ => Left(Problem.pure("Legacy orderIdToBoardState, but instruction has multiple BoardPaths"))
      })

  def isOrderAtStopPosition(order: Order[Order.State]): Boolean =
    Workflow.isOrderAtStopPosition(order, idToWorkflow.get(order.workflowId))

  final def workflowJob(workflowPosition: WorkflowPosition): Checked[WorkflowJob] =
    for {
      workflow <- idToWorkflow.checked(workflowPosition.workflowId)
      job <- workflow.checkedWorkflowJob(workflowPosition.position)
    } yield job

  private def keyToJob(jobKey: JobKey): Checked[WorkflowJob] =
    idToWorkflow.checked(jobKey.workflowId)
      .flatMap(_.keyToJob.checked(jobKey))

  // COMPATIBLE with v2.3
  protected def orderIdToBoardState(orderId: OrderId): Checked[BoardState] =
    for {
      order <- idToOrder.checked(orderId)
      instr <- instruction_[BoardInstruction](order.workflowPosition)
      boardPath <- instr.referencedBoardPaths.toVector match {
        case Vector(o) => Right(o)
        case _ => Left(Problem.pure("Legacy orderIdToBoardState, but instruction has multiple BoardPaths"))
      }
      boardState <- pathTo(BoardState).checked(boardPath)
    } yield boardState

  def instruction(workflowPosition: WorkflowPosition): Instruction =
    idToWorkflow
      .checked(workflowPosition.workflowId)
      .orThrow
      .instruction(workflowPosition.position)

  final def instruction_[A <: Instruction: ClassTag](workflowPosition: WorkflowPosition)
  : Checked[A] =
    idToWorkflow
      .checked(workflowPosition.workflowId)
      .orThrow
      .instruction_[A](workflowPosition.position)

  final def workflowPositionToLabel(workflowPosition: WorkflowPosition): Checked[Option[Label]] =
    for {
      workflow <- idToWorkflow.checked(workflowPosition.workflowId)
      labeled <- workflow.labeledInstruction(workflowPosition.position)
    } yield labeled.maybeLabel

  def childOrderEnded(order: Order[Order.State], parent: Order[Order.Forked]): Boolean = {
    lazy val endReached = order.state == Order.Ready &&
      order.position.parent.contains(parent.position) &&
      instruction(order.workflowPosition).isInstanceOf[End]
    (order.isAttached || order.isDetached) &&
      order.attachedState == parent.attachedState &&
      (order.state.eq(FailedInFork) || endReached)
  }

  final def isWorkflowSuspended(workflowPath: WorkflowPath): Boolean =
    pathToWorkflowPathControlState.get(workflowPath)
      .exists(_.workflowPathControl.suspended)

  /** A pure (stable, repeatable) Scope. */
  final def toPureScope(order: Order[Order.State]): Checked[Scope] =
    for (orderScopes <- toOrderScopes(order)) yield
      orderScopes.pureOrderScope

  /** An impure (unstable, non-repeatable) Scope. */
  final def toImpureOrderExecutingScope(order: Order[Order.State], now: Timestamp): Checked[Scope] =
    for (orderScopes <- toOrderScopes(order)) yield {
      val nowScope = NowScope(now)
      orderScopes.pureOrderScope |+| nowScope |+|
        JobResourceScope(keyTo(JobResource),
          useScope = orderScopes.variablelessOrderScope |+| nowScope)
    }

  final def toOrderScopes(order: Order[Order.State]): Checked[OrderScopes] =
    for (w <- idToWorkflow.checked(order.workflowId)) yield
      OrderScopes(order, w, controllerId)
}
