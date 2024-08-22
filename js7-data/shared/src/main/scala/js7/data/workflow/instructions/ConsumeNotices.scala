package js7.data.workflow.instructions

import io.circe.Codec
import io.circe.derivation.ConfiguredCodec
import js7.base.problem.{Checked, Problem}
import js7.data.board.{BoardPath, BoardPathExpression}
import js7.data.order.OrderEvent.OrderNoticesConsumptionStarted
import js7.data.order.{Order, OrderEvent}
import js7.data.source.SourcePos
import js7.data.workflow.Workflow
import js7.data.workflow.position.{BranchId, Position}

final case class ConsumeNotices(
  boardPaths: BoardPathExpression,
  subworkflow: Workflow,
  sourcePos: Option[SourcePos] = None)
extends ExpectOrConsumeNoticesInstruction:

  def withoutSourcePos: ConsumeNotices =
    copy(sourcePos = None)

  override def withPositions(position: Position): ConsumeNotices =
    copy(
      subworkflow = subworkflow.withPositions(position / BranchId.Cycle))

  override def adopt(outer: Workflow): ConsumeNotices =
    copy(
      subworkflow = subworkflow.copy(
        outer = Some(outer)))

  def referencedBoardPaths: Set[BoardPath] =
    boardPaths.boardPaths

  def fulfilledEvents(
    order: Order[Order.Ready | Order.ExpectingNotices],
    expected: Vector[OrderNoticesConsumptionStarted.Consumption])
  : List[OrderEvent.OrderActorEvent] =
    OrderNoticesConsumptionStarted(expected) :: Nil

  override def workflows: Seq[Workflow] =
    subworkflow :: Nil

  override def branchWorkflows: Seq[(BranchId, Workflow)] =
    (BranchId.ConsumeNotices -> subworkflow) :: Nil

  override def workflow(branchId: BranchId): Checked[Workflow] =
    if branchId != BranchId.ConsumeNotices then
      Left(Problem.pure(s"'${BranchId.ConsumeNotices}' BranchId expected"))
    else
      Right(subworkflow)


object ConsumeNotices:
  implicit val jsonCodec: Codec.AsObject[ConsumeNotices] =
    ConfiguredCodec.derive(useDefaults = true)
