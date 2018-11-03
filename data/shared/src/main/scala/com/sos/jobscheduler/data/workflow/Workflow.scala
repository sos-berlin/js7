package com.sos.jobscheduler.data.workflow

import cats.data.Validated.{Invalid, Valid}
import cats.syntax.option.catsSyntaxOptionId
import com.sos.jobscheduler.base.circeutils.CirceUtils.CirceUtilsChecked
import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.base.utils.Collections.emptyToNone
import com.sos.jobscheduler.base.utils.Collections.implicits.{RichIndexedSeq, RichPairTraversable}
import com.sos.jobscheduler.base.utils.ScalaUtils.{RichJavaClass, RichPartialFunction}
import com.sos.jobscheduler.base.utils.ScalazStyle.OptionRichBoolean
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.filebased.{FileBased, FileBasedId, VersionId}
import com.sos.jobscheduler.data.folder.FolderPath
import com.sos.jobscheduler.data.job.JobKey
import com.sos.jobscheduler.data.workflow.Instruction._
import com.sos.jobscheduler.data.workflow.Workflow.isCorrectlyEnded
import com.sos.jobscheduler.data.workflow.instructions.Instructions.jsonCodec
import com.sos.jobscheduler.data.workflow.instructions.executable.WorkflowJob
import com.sos.jobscheduler.data.workflow.instructions.{End, Execute, ForkJoin, Gap, Goto, If, IfNonZeroReturnCodeGoto, ImplicitEnd}
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, JsonObject, ObjectEncoder}
import scala.collection.immutable.{IndexedSeq, Seq}
import scala.language.implicitConversions

/**
  * @author Joacim Zschimmer
  */
final case class Workflow private(
  id: WorkflowId,
  labeledInstructions: IndexedSeq[Instruction.Labeled],
  nameToJob: Map[WorkflowJob.Name, WorkflowJob],
  source: Option[String])
extends FileBased
{
  assert(isCorrectlyEnded(labeledInstructions), "Missing implicit end instruction")

  type Self = Workflow

  val companion = Workflow

  def withId(id: FileBasedId[WorkflowPath]) = copy(id = id)

  val instructions: IndexedSeq[Instruction] = labeledInstructions map (_.instruction)
  private val labelToNumber: Map[Label, InstructionNr] =
    numberedInstructions.flatMap { case (nr, Labeled(labels, _)) ⇒ labels map (_ → nr) }
      .uniqueToMap(labels ⇒ throw new IllegalArgumentException(s"Duplicate labels in Workflow: ${labels mkString ","}"))

  private def checked: Checked[Workflow] = {
    val problems = labeledInstructions.map (_.instruction).collect {
      case o: Execute.Named if !nameToJob.contains(o.name) ⇒
        Problem.fromEager(s"Undefined job '${o.name}'")

      case jump: JumpInstruction if !labelToNumber.contains(jump.to) ⇒
        Problem.fromEager(s"Unknown label '${jump.to}'")
    }
    if (problems.nonEmpty)
      Invalid(Problem.Multiple(problems))
    else
      Valid(this)
  }

  def firstExecutablePosition = Position(0)

  def labelToPosition(parents: List[Position.Parent], label: Label): Option[Position] =
    for {
      workflow ← workflowOption(parents)
      nr ← workflow.labelToNumber.get(label)
    } yield Position(parents, nr)

  def lastWorkflowPosition: WorkflowPosition =
    id /: Position(lastNr)

  def lastNr: InstructionNr =
    instructions.length - 1

  def flattenedInstructions: Seq[(Position, Instruction.Labeled)] =
    flattenedInstructions(Position.Parents.Empty)

  def flattenedInstructions(parents: Position.Parents): Seq[(Position, Instruction.Labeled)] =
    numberedInstructions flatMap { case (nr, labeled)  ⇒ ((parents / nr) → labeled) +: labeled.instruction.flattenedInstructions(parents / nr) }

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
      case o: Execute.Anonymous ⇒ o.job isExecutableOnAgent agentPath
      case o: Execute.Named ⇒ ??? // TODO
      case o: ForkJoin ⇒ o isPartiallyExecutableOnAgent agentPath
    } contains true

  def isStartableOnAgent(position: Position, agentPath: AgentPath): Boolean =
    isStartableOnAgent(instruction(position), agentPath)

  private def isStartableOnAgent(instruction: Instruction, agentPath: AgentPath): Boolean =
    instruction match {
      case o: ForkJoin ⇒ o.isStartableOnAgent(agentPath)
      case o: Execute.Anonymous ⇒ o.job.isExecutableOnAgent(agentPath)
      case o: Execute.Named ⇒ ???  // TODO
      case _ ⇒ false
    }

  def isStartableOnAgent(agentPath: AgentPath): Boolean =
    checkedWorkflowJob(Position(0)) exists (_ isExecutableOnAgent agentPath)

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
          case instr: If ⇒ instr.workflow(branch) exists (_ isDefinedAt Position(tail, tailNr))
          case _ ⇒ false
        }
    }

  def isDefinedAt(nr: InstructionNr): Boolean =
    labeledInstructions.indices isDefinedAt nr.number

  def keyAndJobs: Seq[(JobKey, WorkflowJob)] =
    nameToJob.toVector.map { case (k, v) ⇒ JobKey.Named(id, k) → v } ++
      flattenedInstructions.collect {
        case (pos, _ @: (ex: Execute.Anonymous)) ⇒ JobKey.Anonymous(id /: pos) → ex.job
      }

  def checkedWorkflowJob(position: Position): Checked[WorkflowJob] =
    checkedExecute(position) flatMap {
      //case o: Execute.Job ⇒ Valid(JobKey.Global(o.jobPath))
      case o: Execute.Anonymous ⇒ Valid(o.job)
      case o: Execute.Named ⇒ nameToJob.checked(o.name)
    }

  def checkedExecute(position: Position): Checked[Execute] =
    instruction(position) match {
      case o: Execute ⇒ Valid(o)
      case o ⇒ Invalid(Problem(s"Expected 'Execute' at workflow position $position (not: ${o.getClass.simpleScalaName})"))
    }

  def instruction(position: Position): Instruction =
    position match {
      case Position(Nil, nr) ⇒
        instruction(nr)

      case Position(Position.Parent(nr, branchId) :: tail, tailNr) ⇒
        (instruction(nr), branchId) match {
          case (instr: If, Position.BranchId.Indexed(index)) ⇒
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
          case (o: If, branchId: Position.BranchId.Indexed) ⇒
            o.workflow(branchId).toOption flatMap (_.workflowOption(tail))
          case _ ⇒
            None
        }
      case _ ⇒ None
    }

  def withoutSource = copy(source = None)

  override def toString = (if (path != WorkflowPath.Anonymous) s"$id " else "") +
    s"{ ${labeledInstructions.mkString("; ")} ${nameToJob.map { case (k, v) ⇒ s"define job $k { $v }" }.mkString(" ")}"
}

object Workflow extends FileBased.Companion[Workflow] {
  type ThisFileBased = Workflow
  type Path = WorkflowPath

  implicit val fileBasedsOverview = WorkflowsOverview
  val typedPathCompanion = WorkflowPath
  private val empty = Workflow(FolderPath.Internal.resolve[WorkflowPath]("empty") % VersionId.Anonymous, Vector.empty)

  /** Test only. */
  def single(
    labeledInstruction: Instruction.Labeled,
    nameToJob: Map[WorkflowJob.Name, WorkflowJob] = Map.empty,
    source: Option[String] = None)
  : Workflow =
    anonymous(Vector(labeledInstruction), nameToJob, source)

  def anonymous(
    labeledInstructions: IndexedSeq[Instruction.Labeled],
    nameToJob: Map[WorkflowJob.Name, WorkflowJob] = Map.empty,
    source: Option[String] = None)
  : Workflow =
    apply(WorkflowPath.NoId, labeledInstructions, nameToJob, source)

  def apply(
    id: WorkflowId,
    labeledInstructions: IndexedSeq[Instruction.Labeled],
    nameToJob: Map[WorkflowJob.Name, WorkflowJob] = Map.empty,
    source: Option[String] = None)
  : Workflow =
    checked(id, labeledInstructions, nameToJob, source).orThrow

  def checked(
    id: WorkflowId,
    labeledInstructions: IndexedSeq[Instruction.Labeled],
    nameToJob: Map[WorkflowJob.Name, WorkflowJob] = Map.empty,
    source: Option[String] = None)
  : Checked[Workflow] =
    new Workflow(
      id,
      labeledInstructions = labeledInstructions ++ !isCorrectlyEnded(labeledInstructions) ? (() @: ImplicitEnd),
      nameToJob,
      source)
    .checked

  def of(instructions: Instruction.Labeled*): Workflow =
    if (instructions.isEmpty)
      empty
    else
      of(WorkflowPath.NoId, instructions: _*)

  def of(id: WorkflowId, instructions: Instruction.Labeled*): Workflow =
    Workflow(id, instructions.toVector)

  def isCorrectlyEnded(labeledInstructions: IndexedSeq[Labeled]): Boolean =
    labeledInstructions.nonEmpty &&
      (labeledInstructions.last.instruction.isInstanceOf[End] ||
       labeledInstructions.last.instruction.isInstanceOf[Goto])

  implicit val jsonEncoder: ObjectEncoder[Workflow] = {
    case Workflow(id, instructions, namedJobs, source) ⇒
      JsonObject(
        "id" → (!id.isAnonymous ? id).asJson,
        "instructions" → instructions.reverse.dropWhile(_ == () @: ImplicitEnd).reverse.asJson,
        "jobs" → emptyToNone(namedJobs).asJson,
        "source" → source.asJson)
  }
  implicit val jsonDecoder: Decoder[Workflow] =
    cursor ⇒ for {
      id ← cursor.get[Option[WorkflowId]]("id") map (_ getOrElse WorkflowPath.NoId)
      instructions ← cursor.get[IndexedSeq[Labeled]]("instructions")
      namedJobs ← cursor.get[Option[Map[WorkflowJob.Name, WorkflowJob]]]("jobs") map (_ getOrElse Map.empty)
      source ← cursor.get[Option[String]]("source")
      workflow ← Workflow.checked(id, instructions, namedJobs, source).toDecoderResult
    } yield workflow
}
