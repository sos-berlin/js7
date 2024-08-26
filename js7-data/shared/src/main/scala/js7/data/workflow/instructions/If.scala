package js7.data.workflow.instructions

import io.circe.{Codec, Decoder, Encoder}
import js7.base.problem.Checked.*
import js7.base.problem.{Checked, Problem}
import js7.data.agent.AgentPath
import js7.data.source.SourcePos
import js7.data.value.expression.Expression
import js7.data.workflow.position.{BranchId, Position}
import js7.data.workflow.{Instruction, Workflow}

/**
  * @author Joacim Zschimmer
  */
final case class If(
  predicate: Expression,
  thenWorkflow: Workflow,
  elseWorkflow: Option[Workflow] = None,
  sourcePos: Option[SourcePos] = None)
extends Instruction:

  def withoutSourcePos: If = copy(
    sourcePos = None,
    thenWorkflow = thenWorkflow.withoutSourcePos,
    elseWorkflow = elseWorkflow.map(_.withoutSourcePos))

  override def withPositions(position: Position): If =
    copy(
      thenWorkflow = thenWorkflow.withPositions(position / BranchId.Then),
      elseWorkflow = elseWorkflow.map(_.withPositions(position / BranchId.Else)))

  override def adopt(outer: Workflow): If = copy(
    thenWorkflow = thenWorkflow.copy(outer = Some(outer)),
    elseWorkflow = elseWorkflow.map(_.copy(outer = Some(outer))))

  override def reduceForAgent(agentPath: AgentPath, workflow: Workflow): Instruction =
    copy(
      thenWorkflow = thenWorkflow.reduceForAgent(agentPath),
      elseWorkflow = elseWorkflow.map(_.reduceForAgent(agentPath)))

  def withoutBlocks: If =
    copy(
      thenWorkflow = Workflow.empty,
      elseWorkflow = elseWorkflow.map(_ => Workflow.empty))

  override def workflow(branchId: BranchId): Checked[Workflow] =
    branchId match
      case BranchId.Then => Right(thenWorkflow)
      case BranchId.Else => elseWorkflow toChecked Problem.pure("This If has no 'else' branch")
      case _ => super.workflow(branchId)

  override def branchWorkflows: Seq[(BranchId, Workflow)] =
    (BranchId.Then -> thenWorkflow) :: elseWorkflow.map(BranchId.Else -> _).toList

  override def toString: String =
    s"if ($predicate) $thenWorkflow" + elseWorkflow.fold("")(w => s" else $w") + sourcePosToString


object If:
  implicit val jsonCodec: Codec.AsObject[If] =
    Codec.AsObject.from[If](
      Decoder.forProduct4(
        "predicate",
        "then",
        "else",
        "sourcePos"
      )(If.apply),
      Encoder.forProduct4(
        "predicate",
        "then",
        "else",
        "sourcePos"
      )((o: If) => (o.predicate, o.thenWorkflow, o.elseWorkflow, o.sourcePos)))
