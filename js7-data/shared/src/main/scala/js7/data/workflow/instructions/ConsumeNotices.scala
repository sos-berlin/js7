package js7.data.workflow.instructions

import io.circe.derivation.ConfiguredCodec
import io.circe.{Codec, Decoder, Encoder}
import js7.base.problem.{Checked, Problem}
import js7.base.utils.L3
import js7.data.agent.AgentPath
import js7.data.board.{BoardPath, BoardPathExpression}
import js7.data.order.OrderEvent.{OrderMoved, OrderNoticesConsumptionStarted, OrderNoticesRead}
import js7.data.order.{Order, OrderEvent}
import js7.data.source.SourcePos
import js7.data.workflow.instructions.ConsumeNotices.*
import js7.data.workflow.instructions.ExpectOrConsumeNoticesInstruction.WhenNotAnnounced
import js7.data.workflow.instructions.ExpectOrConsumeNoticesInstruction.WhenNotAnnounced.{SkipWhenNoNotice, Wait}
import js7.data.workflow.position.{BranchId, Position}
import js7.data.workflow.{Instruction, Workflow}

final case class ConsumeNotices(
  boardPaths: BoardPathExpression,
  whenNotAnnounced: WhenNotAnnounced = WhenNotAnnounced.Wait,
  subworkflow: Workflow,
  sourcePos: Option[SourcePos] = None)
extends ExpectOrConsumeNoticesInstruction, Instruction.WithInstructionBlock:

  def withoutSourcePos: ConsumeNotices =
    copy(sourcePos = None)

  override def withPositions(position: Position): ConsumeNotices =
    copy(
      subworkflow = subworkflow.withPositions(position / BranchId.Cycle))

  override def adopt(outer: Workflow): ConsumeNotices =
    copy(
      subworkflow = subworkflow.copy(
        outer = Some(outer)))

  def reduceForAgent(agentPath: AgentPath, workflow: Workflow): Instruction =
    if isVisibleForAgent(agentPath, workflow) then
      copy(
        subworkflow = subworkflow.reduceForAgent(agentPath))
    else
      Gap(sourcePos)  // The agent will never touch this instruction or it subworkflow

  def referencedBoardPaths: Set[BoardPath] =
    boardPaths.boardPaths

  protected def fulfilledEvents(
    order: Order[Order.Ready | Order.ExpectingNotices],
    consumptions: Vector[OrderEvent.OrderNoticesConsumptionStarted.Consumption],
    exprResult: L3)
  : List[OrderNoticesConsumptionStarted | OrderNoticesRead | OrderMoved] =
    exprResult match
      case L3.False => Nil
      case L3.True => OrderNoticesConsumptionStarted(consumptions) :: Nil
      case L3.Unknown =>
        whenNotAnnounced match
          case Wait => Nil

          case SkipWhenNoNotice if consumptions.isEmpty =>
            OrderNoticesRead
              :: OrderMoved(order.position.increment, Some(OrderMoved.NoNotice))
              :: Nil

          case _ =>
            OrderNoticesConsumptionStarted(consumptions) :: Nil

  def withoutBlocks: ConsumeNotices =
    copy(subworkflow = Workflow.empty)

  override def workflows: Seq[Workflow] =
    subworkflow :: Nil

  def branchWorkflows: Seq[(BranchId, Workflow)] =
    (BranchId.ConsumeNotices -> subworkflow) :: Nil

  override def workflow(branchId: BranchId): Checked[Workflow] =
    if branchId != BranchId.ConsumeNotices then
      Left(Problem.pure(s"'${BranchId.ConsumeNotices}' BranchId expected"))
    else
      Right(subworkflow)


object ConsumeNotices:

  def apply(boardPaths: BoardPathExpression)(instructions: Instruction.Labeled*): ConsumeNotices =
    new ConsumeNotices(boardPaths, subworkflow = Workflow.of(instructions*))

  def apply(
    boardPaths: BoardPathExpression,
    whenNotAnnounced: WhenNotAnnounced)
    (instructions: Instruction.Labeled*)
  : ConsumeNotices =
    new ConsumeNotices(boardPaths, whenNotAnnounced, Workflow.of(instructions*))

  given Codec.AsObject[ConsumeNotices] = ConfiguredCodec.derive(useDefaults = true)
