package js7.data.workflow

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, JsonObject}
import js7.base.circeutils.CirceUtils._
import js7.base.problem.{Checked, Problem}
import js7.base.utils.ScalaUtils.syntax._
import js7.data.agent.AgentPath
import js7.data.source.SourcePos
import js7.data.workflow.Instruction.{Labeled, showSourcePos}
import js7.data.workflow.position._
import scala.collection.View
import scala.language.implicitConversions

/**
  * @author Joacim Zschimmer
  */
trait Instruction
{
  def sourcePos: Option[SourcePos]

  def withoutSourcePos: Instruction

  def withPositions(position: Position): Instruction = {
    assert(branchWorkflows.isEmpty, getClass.simpleScalaName + ".withPositions is not implemented")
    this
  }

  def adopt(workflow: Workflow): Instruction =
    this

  def reduceForAgent(agentPath: AgentPath, workflow: Workflow): Instruction =
    this

  def isVisibleForAgent(agentPath: AgentPath, workflow: Workflow): Boolean =
    workflows.exists(_ isVisibleForAgent agentPath)

  def workflows: Seq[Workflow] =
    branchWorkflows.map(_._2)

  def branchWorkflows: Seq[(BranchId, Workflow)] =
    Nil

  final def flattenedWorkflows(parent: Position): View[(BranchPath, Workflow)] =
    branchWorkflows.view
      .flatMap { case (branchId, workflow) => workflow.flattenedWorkflowsOf(parent / branchId) }

  final def flattenedInstructions(parent: Position): View[(Position, Instruction.Labeled)] =
    branchWorkflows.view
      .flatMap { case (branchId, workflow) => workflow.flattenedInstructions(parent / branchId) }

  def workflow(branchId: BranchId): Checked[Workflow] =
    Problem(s"Instruction '${getClass.simpleScalaName}' does not have a nested workflow for branch '$branchId'")

  def toCatchBranchId(branchId: BranchId): Option[BranchId] = None

  final def @:(maybeLabel: Option[Label]) = Labeled(maybeLabel, this)
  final def @:(label: Label) = Labeled(Some(label), this)
  final def @:(label: String) = Labeled(Some(Label(label)), this)
  final def @:(unit: Unit) = Labeled(None, this)

  protected final def sourcePosToString: String =
    showSourcePos ?? s" /*$sourcePos*/"
}

trait JumpInstruction extends Instruction {
  def to: Label
}

object Instruction
{
  private val showSourcePos = sys.props contains "js7.show-source-pos"  // For Debugging

  object @: {
    def unapply(labeled: Labeled) = Some((labeled.maybeLabel, labeled.instruction))
  }

  implicit def toLabeled(instruction: Instruction): Labeled =
    Labeled(None, instruction)

  final case class Labeled private(
    maybeLabel: Option[Label],
    instruction: Instruction,
    maybePosition: Option[Position] = None)
  {
    override def toString = labelString + instruction

    def labelString = maybeLabel.map(o => s"$o: ").mkString

    def withPositions(position: Position) =
      copy(
        maybePosition = Some(position),
        instruction = instruction withPositions position)
  }
  object Labeled
  {
    implicit def jsonEncoder(implicit instrEncoder: Encoder.AsObject[Instruction]): Encoder.AsObject[Labeled] = {
      case Labeled(None, instruction, None) =>
        instruction.asJsonObject

      case Labeled(maybeLabel, instruction, maybePosition) =>
        JsonObject.fromIterable(
          maybeLabel.map(o => "label" -> o.asJson) ++
            maybePosition.map(o => "position" -> o.asJson)
        ) ++ instruction.asJsonObject
    }

    private val rightNode = Right(None)

    implicit def jsonDecoder(implicit instrDecoder: Decoder[Instruction]): Decoder[Labeled] =
      cursor => for {
        instruction <- cursor.as[Instruction]
        labels <- {
          val c1 = cursor.downField("label")
          if (c1.succeeded)
            c1.as[Option[Label]]
          else
            rightNode
        }
      } yield Labeled(labels, instruction)
  }
}
