package com.sos.jobscheduler.data.workflow

import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.base.utils.ScalaUtils.RichJavaClass
import com.sos.jobscheduler.data.workflow.Instruction.Labeled
import com.sos.jobscheduler.data.workflow.position._
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Json, ObjectEncoder}
import scala.collection.immutable.Seq
import scala.language.implicitConversions

/**
  * @author Joacim Zschimmer
  */
trait Instruction
{
  def adopt(workflow: Workflow): Instruction =
    this

  def workflows: Seq[Workflow] = branchWorkflows map (_._2)

  protected def branchWorkflows: Seq[(BranchId, Workflow)] = Nil

  final def flattenedWorkflows(parent: Position): Seq[(BranchPath, Workflow)] =
    branchWorkflows flatMap { case (branchId, workflow) => workflow.flattenedWorkflowsOf(parent / branchId) }

  final def flattenedInstructions(parent: Position): Seq[(Position, Instruction.Labeled)] =
    branchWorkflows flatMap { case (branchId, workflow) => workflow.flattenedInstructions(parent / branchId) }

  def workflow(branchId: BranchId): Checked[Workflow] =
    Problem(s"Instruction '${getClass.simpleScalaName}' does not have a nested workflow for branch '$branchId'")

  def normalizeBranchId(branchId: BranchId) = branchId

  def toCatchBranchId(branchId: BranchId): Option[BranchId] = None

  final def @:(labels: Seq[Label]) = Labeled(labels, this)
  final def @:(label: Label) = Labeled(label :: Nil, this)
  final def @:(label: String) = Labeled(Label(label) :: Nil, this)
  final def @:(unit: Unit) = Labeled(Nil, this)
}

trait JumpInstruction extends Instruction {
  def to: Label
}

object Instruction {
  val @: = Labeled

  implicit def toLabeled(instruction: Instruction): Labeled =
    Labeled(Nil, instruction)

  final case class Labeled(labels: Seq[Label], instruction: Instruction) {
    override def toString = labelsString + instruction

    def labelsString = labels.map(o => s"$o: ").mkString
  }
  object Labeled {
    implicit def jsonEncoder(implicit instrEncoder: ObjectEncoder[Instruction]): ObjectEncoder[Labeled] = {
      case Labeled(Seq(), instruction) =>
        instruction.asJsonObject
      case Labeled(labels, instruction) =>
        ("labels" -> labels.asJson) +: instruction.asJsonObject
    }

    implicit def jsonDecoder(implicit instrDecoder: Decoder[Instruction]): Decoder[Labeled] =
      cursor => for {
        instruction <- cursor.as[Instruction]
        labels <- cursor.get[Json]("labels") match {
          case Right(json) => json.as[Seq[Label]]
          case Left(_) => Right(Nil)
        }
      } yield Labeled(labels, instruction)
  }
}
