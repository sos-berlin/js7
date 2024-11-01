package js7.data.workflow.instructions

import cats.data.NonEmptyVector
import cats.syntax.option.*
import io.circe.syntax.EncoderOps
import io.circe.{Codec, Decoder, DecodingFailure, Encoder, JsonObject}
import js7.base.problem.Checked.*
import js7.base.problem.{Checked, Problem}
import js7.base.utils.CatsUtils.Nev
import js7.data.agent.AgentPath
import js7.data.source.SourcePos
import js7.data.value.expression.Expression
import js7.data.workflow.instructions.If.*
import js7.data.workflow.position.{BranchId, NewBranchId, Position}
import js7.data.workflow.{Instruction, Workflow}

/**
  * @author Joacim Zschimmer
  */
final case class If(
  ifThens: NonEmptyVector[IfThen],
  elseBlock: Option[Workflow] = None,
  sourcePos: Option[SourcePos] = None)
extends Instruction:

  def withoutSourcePos: If = copy(
    sourcePos = None,
    ifThens = ifThens.map(ifThen => ifThen.copy(
      thenBlock = ifThen.thenBlock.withoutSourcePos)),
    elseBlock = elseBlock.map(_.withoutSourcePos))

  override def withPositions(position: Position): If =
    copy(
      ifThens = ifThens.map(ifThen => ifThen.copy(
        thenBlock = ifThen.thenBlock.withPositions(position / BranchId.Then))),
      elseBlock = elseBlock.map(_.withPositions(position / BranchId.Else)))

  override def adopt(outer: Workflow): If = copy(
    ifThens = ifThens.map(ifThen => ifThen.copy(
      thenBlock = ifThen.thenBlock.copy(
        outer = Some(outer)))),
    elseBlock = elseBlock.map(_.copy(
      outer = Some(outer))))

  override def reduceForAgent(agentPath: AgentPath, workflow: Workflow): Instruction =
    copy(
      ifThens = ifThens.map(ifThen => ifThen.copy(
        thenBlock = ifThen.thenBlock.reduceForAgent(agentPath))),
      elseBlock = elseBlock.map(_.reduceForAgent(agentPath)))

  def withoutBlocks: If =
    copy(
      ifThens = ifThens.map(ifThen => ifThen.copy(
        thenBlock = Workflow.empty)),
      elseBlock = elseBlock.map(_ => Workflow.empty))

  override def workflow(branchId: BranchId): Checked[Workflow] =
    NewBranchId.fromLegacy(branchId).flatMap:
      case newBranchId @ NewBranchId.Then(i) =>
        ifThens.get(i - 1).toRight(Problem(s"This if has no $newBranchId")).map(_.thenBlock)
      case NewBranchId.Else =>
        elseBlock.toChecked(Problem.pure("This If has no 'else' branch"))
      case _ =>
        super.workflow(branchId)


  override def branchWorkflows: Seq[(BranchId, Workflow)] =
    ifThens.toVector.zipWithIndex.map: (ifThen, i) =>
      BranchId.then_(1 + i) -> ifThen.thenBlock
    .concat:
      elseBlock.map(BranchId.Else -> _)

  override def toString: String =
    ifThens.toVector.map(_.toString).mkString(" else ")
      .concat:
        elseBlock.fold("")(w => s" else $w")
      .concat:
        sourcePosToString


object If:

  def apply(predicate: Expression)(thenInstr: Instruction.Labeled): If =
    apply(predicate)(Workflow.of(thenInstr))

  def apply(predicate: Expression)(thenBlock: Workflow): If =
    new If(Nev.one(IfThen(predicate, thenBlock)))

  def apply(predicate: Expression): Then =
    If.Then(Vector.empty, predicate)

  final case class Then(ifThens: Vector[IfThen], predicate: Expression):
    def Then(instruction: Instruction.Labeled): Else =
      Then(Workflow.of(instruction))

    def Then(thenBlock: Workflow): Else =
      If.Else:
        Nev.fromVectorUnsafe:
          ifThens :+ IfThen(predicate, thenBlock)

  final case class Else(ifThens: Nev[IfThen]):
    def elseIf(predicate: Expression): Then =
      Then(ifThens.toVector, predicate)

    def Else(instruction: Instruction.Labeled): If =
      Else(Workflow.of(instruction))

    def Else(elseBlock: Workflow): If =
      new If(ifThens, elseBlock.some)


  def apply(predicate: Expression, thenBlock: Workflow, sourcePos: SourcePos): If =
    new If(Nev.one(IfThen(predicate, thenBlock)), sourcePos = sourcePos.some)

  final case class IfThen(predicate: Expression, thenBlock: Workflow):
    override def toString = s"if ($predicate) $thenBlock"

  object IfThen:
    given Codec.AsObject[IfThen] =
      Codec.AsObject.from[IfThen](
        Decoder.forProduct2("predicate", "then")(IfThen.apply),
        Encoder.forProduct2("predicate", "then")((o: IfThen) => (o.predicate, o.thenBlock)))

  given Codec.AsObject[If] =
    Codec.AsObject.from[If](
      c =>
        for
          ifThens <- c.getOrElse[Vector[IfThen]]("ifThens")(Vector.empty)
          predicate <- c.get[Option[Expression]]("predicate") // COMPATIBLE with v2.7.1
          then_ <- c.get[Option[Workflow]]("then") // COMPATIBLE with v2.7.1
          else_ <- c.get[Option[Workflow]]("else")
          sourcePos <- c.get[Option[SourcePos]]("sourcePos")
          elseIf <- (predicate, then_) match
            case (None, None) if ifThens.nonEmpty =>
              Right(Nev.fromVectorUnsafe(ifThens))
            case (Some(predicate), Some(then_)) if ifThens.isEmpty =>
              Right(Nev.one(IfThen(predicate, then_)))
            case _ =>
              Left(DecodingFailure("Invalid If instruction JSON encoding", c.history))
        yield
          If(elseIf, else_, sourcePos),
      o => JsonObject(
        "ifThens" -> o.ifThens.toVector.asJson,
        "else" -> o.elseBlock.asJson,
        "sourcePos" -> o.sourcePos.asJson))
