package com.sos.jobscheduler.data.workflow

import cats.data.Validated.{Invalid, Valid}
import cats.syntax.option.catsSyntaxOptionId
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.base.utils.Collections.implicits.{RichIndexedSeq, RichPairTraversable}
import com.sos.jobscheduler.base.utils.ScalaUtils.RichJavaClass
import com.sos.jobscheduler.base.utils.ScalazStyle.OptionRichBoolean
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.filebased.FileBased
import com.sos.jobscheduler.data.folder.FolderPath
import com.sos.jobscheduler.data.workflow.Instruction._
import com.sos.jobscheduler.data.workflow.instructions.Instructions.jsonCodec
import com.sos.jobscheduler.data.workflow.instructions.{End, ForkJoin, Gap, Goto, IfNonZeroReturnCodeGoto, IfReturnCode, ImplicitEnd, Job}
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, JsonObject, ObjectEncoder}
import scala.collection.immutable.{IndexedSeq, Seq}
import scala.language.implicitConversions
import com.sos.jobscheduler.base.utils.Strings._

/**
  * @author Joacim Zschimmer
  */
final case class Workflow private(path: WorkflowPath, labeledInstructions: IndexedSeq[Instruction.Labeled], source: Option[String])
extends FileBased
{
  import com.sos.jobscheduler.data.workflow.Workflow._

  type Self = Workflow

  def companion = Workflow

  assert(isCorrectlyEnded(labeledInstructions), "Missing implicit end instruction")

  val instructions: IndexedSeq[Instruction] = labeledInstructions map (_.instruction)
  private val _labelToNumber: Map[Label, InstructionNr] =
    numberedInstructions.flatMap { case (nr, Labeled(labels, _)) ⇒ labels map (_ → nr) }
      .uniqueToMap(labels ⇒ throw new IllegalArgumentException(s"Duplicate labels in Workflow: ${labels mkString ","}"))

  labeledInstructions foreach {
    case _ @: (jump: JumpInstruction) ⇒
      _labelToNumber.getOrElse(jump.to, throw new IllegalArgumentException(s"Unknown label '${jump.to}'"))
    case _ ⇒
  }

  def firstExecutablePosition = Position(0)

  def labelToPosition(parents: List[Position.Parent], label: Label): Option[Position] =
    for {
      workflow ← workflowOption(parents)
      nr ← workflow._labelToNumber.get(label)
    } yield Position(parents, nr)

  def lastWorkflowPosition: WorkflowPosition =
    path /: Position(lastNr)

  def lastNr: InstructionNr =
    instructions.length - 1

  def numberedInstructions: Seq[(InstructionNr, Instruction.Labeled)] =
    labeledInstructions.zipWithIndex.map {
      case (s, i) ⇒ InstructionNr(i) → s
    }

  def reduce: Workflow =
    copy(labeledInstructions =
      labeledInstructions.sliding(2).flatMap { // Peep-hole optimize
        case Seq(_ @: (jmp: JumpInstruction), Labeled(labels, _)) if labels contains jmp.to ⇒ Nil
        case Seq(_ @: IfNonZeroReturnCodeGoto(errorTo), _ @: Goto(to)) if errorTo == to ⇒ Nil
        case Seq(a, _) ⇒ a :: Nil
        case Seq(_) ⇒ Nil  // Unused code in contrast to sliding's documentation?
      }.toVector ++
        labeledInstructions.lastOption)

  def isPartiallyExecutableOnAgent(agentPath: AgentPath): Boolean =
    labeledInstructions map (_.instruction) collect {
      case o: Job ⇒ o isExecutableOnAgent agentPath
      case o: ForkJoin ⇒ o isPartiallyExecutableOnAgent agentPath
    } contains true

  def isStartableOnAgent(position: Position, agentPath: AgentPath): Boolean =
    isStartableOnAgent(instruction(position), agentPath)

  private def isStartableOnAgent(instruction: Instruction, agentPath: AgentPath): Boolean =
    instruction match {
      case o: ForkJoin ⇒ o.isStartableOnAgent(agentPath)
      case o: Job ⇒ o.isExecutableOnAgent(agentPath)
      case _ ⇒ false
    }

  def isStartableOnAgent(agentPath: AgentPath): Boolean =
    instructions.headOption collect { case o: Job ⇒ o } exists (_ isExecutableOnAgent agentPath)

  //def isEndingOnAgent(agentPath: AgentPath): Boolean =
  //  labeledInstructions.reverse.dropWhile(_.isInstanceOf[End]).headOption collect { case o: Job ⇒ o } exists (_ isExecutableOnAgent agentPath)

  def isDefinedAt(position: Position): Boolean =
    position match {
      case Position(Nil, nr) ⇒ isDefinedAt(nr)
      case Position(Position.Parent(nr, branch: Position.BranchId.Named) :: tail, tailNr) ⇒
        instruction(nr) match {
          case fj: ForkJoin ⇒ fj.workflowOption(branch) exists (_ isDefinedAt Position(tail, tailNr))
          case _ ⇒ false
        }
      case Position(Position.Parent(nr, branch: Position.BranchId.Indexed) :: tail, tailNr) ⇒
        instruction(nr) match {
          case instr: IfReturnCode ⇒ instr.workflow(branch) exists (_ isDefinedAt Position(tail, tailNr))
          case _ ⇒ false
        }
    }

  def isDefinedAt(nr: InstructionNr): Boolean =
    labeledInstructions.indices isDefinedAt nr.number

  def checkedJob(position: Position): Checked[Job] =
    instruction(position) match {
      case o: Job ⇒ Valid(o)
      case o ⇒ Invalid(Problem(s"Expected a Job at workflow position $position (not: ${o.getClass.simpleScalaName})"))
    }

  def jobOption(position: Position): Option[Job] =
    Some(instruction(position)) collect { case o: Job ⇒ o }

  def instruction(position: Position): Instruction =
    position match {
      case Position(Nil, nr) ⇒
        instruction(nr)

      case Position(Position.Parent(nr, branchId) :: tail, tailNr) ⇒
        (instruction(nr), branchId) match {
          case (instr: IfReturnCode, Position.BranchId.Indexed(index)) ⇒
            instr.workflow(index) map (_.instruction(Position(tail, tailNr))) getOrElse Gap
          case (fj: ForkJoin, branchId: Position.BranchId.Named) ⇒
            fj.workflowOption(branchId) map (_.instruction(Position(tail, tailNr))) getOrElse Gap
          case _ ⇒
            Gap
        }
    }

  def labeledInstruction(position: Position): Instruction.Labeled =
    workflowOption(position.parents).flatMap(_.labeledInstructions.get(position.nr.number))
      .getOrElse(throw new IllegalArgumentException(s"Unknown workflow position $position"))

  def instruction(nr: InstructionNr): Instruction =
    if (instructions.indices contains nr.number)
      instructions(nr.number)
    else
      Gap

  def workflowOption(position: Position): Option[Workflow] =
    workflowOption(position.parents)

  private def workflowOption(parents: List[Position.Parent]): Option[Workflow] =
    parents match {
      case Nil ⇒ this.some
      case Position.Parent(nr, branchId: Position.BranchId) :: tail ⇒
        (instruction(nr), branchId) match {
          case (o: ForkJoin, branchId: Position.BranchId.Named) ⇒
            o.workflowOption(branchId) flatMap (_.workflowOption(tail))
          case (o: IfReturnCode, branchId: Position.BranchId.Indexed) ⇒
            o.workflow(branchId).toOption flatMap (_.workflowOption(tail))
          case _ ⇒
            None
        }
      case _ ⇒ None
    }

  def withoutSource = copy(source = None)

  override def toString = (if (path != WorkflowPath.Anonymous) s"$path " else "") + s"{ ${labeledInstructions.mkString("; ")} }"
}

object Workflow extends FileBased.Companion[Workflow] {
  type ThisFileBased = Workflow
  type ThisTypedPath = WorkflowPath

  val typedPathCompanion = WorkflowPath
  private val empty = Workflow(FolderPath.Internal.resolve[WorkflowPath]("empty"), Vector.empty)

  def apply(path: WorkflowPath, labeledInstructions: IndexedSeq[Instruction.Labeled], source: Option[String] = None): Workflow =
    new Workflow(
      path,
      labeledInstructions = labeledInstructions ++ !isCorrectlyEnded(labeledInstructions) ? (() @: ImplicitEnd),
      source)

  def of(instructions: Instruction.Labeled*): Workflow =
    if (instructions.isEmpty)
      empty
    else
      of(WorkflowPath.Anonymous, instructions: _*)

  def of(path: WorkflowPath, instructions: Instruction.Labeled*): Workflow =
    Workflow(path, instructions.toVector)

  def isCorrectlyEnded(labeledInstructions: IndexedSeq[Labeled]): Boolean =
    labeledInstructions.nonEmpty &&
      (labeledInstructions.last.instruction.isInstanceOf[End] ||
       labeledInstructions.last.instruction.isInstanceOf[Goto])

  implicit val jsonEncoder: ObjectEncoder[Workflow] = {
    case Workflow(path, instructions, source) ⇒
      JsonObject(
        "path" → (!path.isGenerated ? path).asJson,
        "instructions" → instructions.asJson,
        "source" → source.asJson)
  }
  implicit val jsonDecoder: Decoder[Workflow] =
    cursor ⇒ for {
      path ← cursor.get[Option[WorkflowPath]]("path") map (_ getOrElse WorkflowPath.Anonymous)
      instructions ← cursor.get[IndexedSeq[Labeled]]("instructions")
      source ← cursor.get[Option[String]]("source")
    } yield Workflow(path, instructions, source)
}
