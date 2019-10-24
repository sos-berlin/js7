package com.sos.jobscheduler.data.workflow

import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.base.utils.ScalaUtils.RichJavaClass
import com.sos.jobscheduler.data.source.SourcePos
import com.sos.jobscheduler.data.workflow.Instruction.Labeled
import com.sos.jobscheduler.data.workflow.position._
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, Json}
import scala.collection.immutable.Seq
import scala.language.implicitConversions

/**
  * @author Joacim Zschimmer
  */
trait Instruction
{
  def sourcePos: Option[SourcePos]

  def withoutSourcePos: Instruction

  def adopt(workflow: Workflow): Instruction =
    this

  def workflows: Seq[Workflow] = branchWorkflows map (_._2)

  def branchWorkflows: Seq[(BranchId, Workflow)] = Nil

  final def flattenedWorkflows(parent: Position): Seq[(BranchPath, Workflow)] =
    branchWorkflows flatMap { case (branchId, workflow) => workflow.flattenedWorkflowsOf(parent / branchId) }

  final def flattenedInstructions(parent: Position): Seq[(Position, Instruction.Labeled)] =
    branchWorkflows flatMap { case (branchId, workflow) => workflow.flattenedInstructions(parent / branchId) }

  def workflow(branchId: BranchId): Checked[Workflow] =
    Problem(s"Instruction '${getClass.simpleScalaName}' does not have a nested workflow for branch '$branchId'")

  def toCatchBranchId(branchId: BranchId): Option[BranchId] = None

  final def @:(maybeLabel: Option[Label]) = Labeled(maybeLabel, this)
  final def @:(label: Label) = Labeled(Some(label), this)
  final def @:(label: String) = Labeled(Some(Label(label)), this)
  final def @:(unit: Unit) = Labeled(None, this)
}

trait JumpInstruction extends Instruction {
  def to: Label
}

object Instruction {
  val @: = Labeled

  implicit def toLabeled(instruction: Instruction): Labeled =
    Labeled(None, instruction)

  final case class Labeled(maybeLabel: Option[Label], instruction: Instruction) {
    override def toString = labelString + instruction

    def labelString = maybeLabel.map(o => s"$o: ").mkString
  }
  object Labeled {
    implicit def jsonEncoder(implicit instrEncoder: Encoder.AsObject[Instruction]): Encoder.AsObject[Labeled] = {
      case Labeled(None, instruction) =>
        instruction.asJsonObject
      case Labeled(maybeLabel, instruction) =>
        ("label" -> maybeLabel.asJson) +: instruction.asJsonObject
    }

    implicit def jsonDecoder(implicit instrDecoder: Decoder[Instruction]): Decoder[Labeled] =
      cursor => for {
        instruction <- cursor.as[Instruction]
        labels <- cursor.get[Json]("label") match {
          case Right(json) => json.as[Option[Label]]
          case Left(_) => Right(None)
        }
      } yield Labeled(labels, instruction)
  }
}
