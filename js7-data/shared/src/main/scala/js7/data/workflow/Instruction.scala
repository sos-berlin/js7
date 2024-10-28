package js7.data.workflow

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, JsonObject}
import js7.base.circeutils.CirceUtils.*
import js7.base.problem.{Checked, Problem}
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.Tests.isTest
import js7.data.agent.AgentPath
import js7.data.source.SourcePos
import js7.data.workflow.Instruction.Labeled
import js7.data.workflow.position.*
import scala.collection.View
import scala.language.implicitConversions

/**
  * @author Joacim Zschimmer
  */
trait Instruction:

  def instructionName: String =
    getClass.simpleScalaName

  def sourcePos: Option[SourcePos]

  def withoutSourcePos: Instruction

  def withPositions(position: Position): Instruction =
    if branchWorkflows.nonEmpty then throw new AssertionError(
      s"$instructionName.withPositions is not implemented")
    this

  def adopt(workflow: Workflow): Instruction =
    this

  def reduceForAgent(agentPath: AgentPath, workflow: Workflow): Instruction =
    this

  def isVisibleForAgent(agentPath: AgentPath, workflow: Workflow): Boolean =
    workflows.exists(_.isVisibleForAgent(agentPath))

  def withoutBlocks: Instruction

  def branchWorkflows: Seq[(BranchId, Workflow)]

  // The instruction blocks of the instruction
  def workflows: Seq[Workflow] =
    branchWorkflows.map(_._2)

  final def flattenedWorkflows(parent: Position): View[(BranchPath, Workflow)] =
    branchWorkflows.view
      .flatMap { case (branchId, workflow) => workflow.flattenedWorkflowsOf(parent / branchId) }

  final def flattenedInstructions(parent: Position): View[(Position, Instruction.Labeled)] =
    branchWorkflows.view
      .flatMap { case (branchId, workflow) => workflow.flattenedInstructions(parent / branchId) }

  def workflow(branchId: BranchId): Checked[Workflow] =
    Problem(s"Instruction '$instructionName' does not have a nested workflow for branch '$branchId'")

  def toCatchBranchId(branchId: BranchId): Option[BranchId] =
    None

  /** Compares the instructions without sourcePos or instruction blocks. */
  final def isSameCore(other: Instruction): Boolean =
    withoutSourcePos.withoutBlocks == other.withoutSourcePos.withoutBlocks

  final def @:(maybeLabel: Option[Label]): Labeled =
    Labeled(maybeLabel, this)

  final def @:(label: Label): Labeled =
    Labeled(Some(label), this)

  final def @:(label: String): Labeled =
    Labeled(Some(Label(label)), this)

  final def @:(unit: Unit): Labeled =
    Labeled(None, this)

  protected final def sourcePosToString: String =
    isTest ?? s" /*$sourcePos*/"


object Instruction:
  object `@:`:
    def unapply(labeled: Labeled): Some[(Option[Label], Instruction)] = Some((labeled.maybeLabel, labeled.instruction))

  implicit def toLabeled(instruction: Instruction): Labeled =
    Labeled(None, instruction)

  final case class Labeled(
    maybeLabel: Option[Label],
    instruction: Instruction,
    maybePosition: Option[Position] = None):
    override def toString: String = labelString + instruction

    def labelString: String =
      maybeLabel.map(o => s"$o: ").mkString

    def withPositions(position: Position): Labeled =
      copy(
        maybePosition = Some(position),
        instruction = instruction.withPositions(position))
  object Labeled:
    implicit def jsonEncoder(implicit instrEncoder: Encoder.AsObject[Instruction]): Encoder.AsObject[Labeled] =
      case Labeled(None, instruction, None) =>
        instruction.asJsonObject

      case Labeled(maybeLabel, instruction, maybePosition) =>
        JsonObject.fromIterable(
          maybeLabel.map(o => "label" -> o.asJson) ++
            maybePosition.map(o => "position" -> o.asJson)
        ) ++ instruction.asJsonObject

    private val rightNode = Right(None)

    implicit def jsonDecoder(implicit instrDecoder: Decoder[Instruction]): Decoder[Labeled] =
      cursor => for
        instruction <- cursor.as[Instruction]
        labels <-
          val c1 = cursor.downField("label")
          if c1.succeeded then
            c1.as[Option[Label]]
          else
            rightNode
      yield Labeled(labels, instruction)

  trait IsOrderBoundary extends Instruction

  /** Instruction has no own instruction block (is not nesting). */
  trait NoInstructionBlock extends Instruction:
    final def withoutBlocks: NoInstructionBlock =
      this

    final def branchWorkflows: Seq[(BranchId, Workflow)] =
      Nil
