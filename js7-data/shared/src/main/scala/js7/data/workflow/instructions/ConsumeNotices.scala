package js7.data.workflow.instructions

import io.circe.Codec
import io.circe.derivation.ConfiguredCodec
import js7.base.problem.{Checked, Problem}
import js7.data.board.BoardPathExpression
import js7.data.order.Order
import js7.data.order.OrderEvent.OrderNoticesConsumptionStarted
import js7.data.source.SourcePos
import js7.data.workflow.position.{BranchId, Position}
import js7.data.workflow.{Instruction, Workflow}

final case class ConsumeNotices(
  boardPaths: BoardPathExpression,
  subworkflow: Workflow,
  sourcePos: Option[SourcePos] = None)
extends ExpectOrConsumeNoticesInstruction:
  def withoutSourcePos =
    copy(sourcePos = None)

  override def withPositions(position: Position): Instruction =
    copy(
      subworkflow = subworkflow.withPositions(position / BranchId.Cycle))

  override def adopt(outer: Workflow) = copy(
    subworkflow = subworkflow.copy(
      outer = Some(outer)))

  def referencedBoardPaths =
    boardPaths.boardPaths

  def fulfilledEvents(
    order: Order[Order.State],
    consumptions: Vector[OrderNoticesConsumptionStarted.Consumption])
  = OrderNoticesConsumptionStarted(consumptions) :: Nil

  override def workflows =
    subworkflow :: Nil

  override def branchWorkflows =
    (BranchId.ConsumeNotices -> subworkflow) :: Nil

  override def workflow(branchId: BranchId): Checked[Workflow] =
    if branchId != BranchId.ConsumeNotices then
      Left(Problem.pure(s"'${BranchId.ConsumeNotices}' BranchId expected"))
    else
      Right(subworkflow)

object ConsumeNotices:
  implicit val jsonCodec: Codec.AsObject[ConsumeNotices] =
    ConfiguredCodec.derive(useDefaults = true)
