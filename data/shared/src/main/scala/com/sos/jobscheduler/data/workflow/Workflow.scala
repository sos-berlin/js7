package com.sos.jobscheduler.data.workflow

import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.base.utils.Collections.emptyToNone
import com.sos.jobscheduler.base.utils.Collections.implicits.{RichIndexedSeq, RichPairTraversable}
import com.sos.jobscheduler.base.utils.ScalaUtils.{RichJavaClass, reuseIfEqual}
import com.sos.jobscheduler.base.utils.ScalazStyle.OptionRichBoolean
import com.sos.jobscheduler.data.agent.AgentRefPath
import com.sos.jobscheduler.data.filebased.{FileBased, FileBasedId}
import com.sos.jobscheduler.data.job.JobKey
import com.sos.jobscheduler.data.workflow.Instruction.@:
import com.sos.jobscheduler.data.workflow.Workflow.isCorrectlyEnded
import com.sos.jobscheduler.data.workflow.instructions.Instructions.jsonCodec
import com.sos.jobscheduler.data.workflow.instructions.executable.WorkflowJob
import com.sos.jobscheduler.data.workflow.instructions.{End, Execute, Fork, Gap, Goto, If, IfNonZeroReturnCodeGoto, ImplicitEnd, Retry, TryInstruction}
import com.sos.jobscheduler.data.workflow.position.{BranchPath, InstructionNr, Position, WorkflowBranchPath, WorkflowPosition}
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, JsonObject, ObjectEncoder}
import scala.annotation.tailrec
import scala.collection.immutable.{IndexedSeq, Seq}

/**
  * @author Joacim Zschimmer
  */
final case class Workflow private(
  id: WorkflowId,
  rawLabeledInstructions: IndexedSeq[Instruction.Labeled],
  nameToJob: Map[WorkflowJob.Name, WorkflowJob],
  source: Option[String],
  outer: Option[Workflow])
extends FileBased
{
  override def equals(o: Any) = o match {
    case o: Workflow => id == o.id && rawLabeledInstructions == o.rawLabeledInstructions && nameToJob == o.nameToJob && source == o.source  // Ignore outer ???
    case _ => false
  }

  assert(isCorrectlyEnded(rawLabeledInstructions), "Missing implicit end instruction")

  type Self = Workflow

  val companion = Workflow
  val labeledInstructions = rawLabeledInstructions map (o => o.copy(instruction = o.instruction.adopt(this)))
  val instructions: IndexedSeq[Instruction] = labeledInstructions map (_.instruction)
  private val labelToNumber: Map[Label, InstructionNr] =
    numberedInstructions.flatMap { case (nr, Instruction.Labeled(labels, _)) => labels map (_ -> nr) }
      .uniqueToMap(labels => throw new IllegalArgumentException(s"Duplicate labels in Workflow: ${labels mkString ","}"))

  def withId(id: FileBasedId[WorkflowPath]) = reuseIfEqual(this, copy(id = id))

  private def checked: Checked[Workflow] = {
    val problems = labeledInstructions.map (_.instruction).collect {
      case jump: JumpInstruction if !labelToNumber.contains(jump.to) =>
        Problem.pure(s"Unknown label '${jump.to}'")
    }
    if (problems.nonEmpty)
      Invalid(Problem.Multiple(problems))
    else
      Valid(this)
  }

  /** Checks a complete workflow including subworkflows using jobs in its outer workflows. */
  def completelyChecked: Checked[Workflow] = {
    val chk = flattenedWorkflows.map(_._2.checked) ++
      flattenedInstructions.collect {
        case (position, _ @: (o: Execute.Named)) => nestedWorkflow(position.branchPath).flatMap(_.findJob(o.name))
      } ++
      checkRetry(false)
    val problems = chk collect { case Invalid(problem) => problem }
    if (problems.nonEmpty)
      Invalid(Problem.Multiple(problems))
    else
      Valid(this)
  }

  private def checkRetry(inCatch: Boolean): Seq[Checked[Unit]] =
    instructions flatMap {
      case instr: TryInstruction =>
        Vector(instr.tryWorkflow.checkRetry(inCatch), instr.catchWorkflow.checkRetry(true)).flatten
      case _: Retry =>
        !inCatch thenList Invalid(Problem("Statement 'retry' is possible only in a catch block"))
      case instr: If =>
        instr.workflows flatMap (_.checkRetry(inCatch))
      case instr =>
        instr.workflows flatMap (_.checkRetry(false))
    }

  def labelToPosition(branchPath: BranchPath, label: Label): Checked[Position] =
    for {
      workflow <- nestedWorkflow(branchPath)
      nr <- workflow.labelToNumber.get(label).toChecked(Problem(s"Unknown label '$label'"))
    } yield branchPath % nr

  def lastWorkflowPosition: WorkflowPosition =
    id /: Position(lastNr)

  def lastNr: InstructionNr =
    instructions.length - 1

  def anonymousJobKey(workflowPosition: WorkflowPosition): JobKey.Anonymous =
    JobKey.Anonymous(workflowPosition.normalized)

  private[workflow] def flattenedWorkflows: Map[BranchPath, Workflow] =
    flattenedWorkflowsOf(Nil).toMap

  private[workflow] def flattenedWorkflowsOf(parents: BranchPath): List[(BranchPath, Workflow)] =
    (parents -> this) :: nestedWorkflows(parents)

  private def nestedWorkflows(parents: BranchPath): List[(BranchPath, Workflow)] =
    numberedInstructions.toList flatMap {
      case (nr, labeled) => labeled.instruction.flattenedWorkflows(parents % nr)
    }

  private[workflow] def nestedWorkflow(branchPath: BranchPath): Checked[Workflow] =
    branchPath match {
      case Nil => Valid(this)
      case BranchPath.PositionAndBranchId(position, branchId) => instruction(position).workflow(branchId)
    }

  private[workflow] def flattenedInstructions: Seq[(Position, Instruction.Labeled)] =
    flattenedInstructions(Nil)

  private[workflow] def flattenedInstructions(branchPath: BranchPath): Seq[(Position, Instruction.Labeled)] =
    numberedInstructions flatMap {
      case (nr, labeled) => ((branchPath % nr) -> labeled) +: labeled.instruction.flattenedInstructions(branchPath % nr)
    }

  def numberedInstructions: Seq[(InstructionNr, Instruction.Labeled)] =
    labeledInstructions.zipWithIndex.map {
      case (s, i) => InstructionNr(i) -> s
    }

  private[workflow] def reduce: Workflow =
    copy(rawLabeledInstructions =
      rawLabeledInstructions.sliding(2).flatMap { // Peep-hole optimize
        case Seq(_ @: (jmp: JumpInstruction), Instruction.Labeled(labels, _)) if labels contains jmp.to => Nil
        case Seq(_ @: IfNonZeroReturnCodeGoto(errorTo), _ @: Goto(to)) if errorTo == to => Nil
        case Seq(a, _) => a :: Nil
        case Seq(_) => Nil  // Unused code in contrast to sliding's documentation?
      }.toVector ++
        rawLabeledInstructions.lastOption)

  private[workflow] def isPartiallyExecutableOnAgent(agentRefPath: AgentRefPath): Boolean =
    labeledInstructions map (_.instruction) collect {
      case o: Execute.Anonymous => o.job isExecutableOnAgent agentRefPath
      case o: Execute.Named => findJob(o.name) exists (_ isExecutableOnAgent agentRefPath)
      case o: Fork => o isPartiallyExecutableOnAgent agentRefPath
    } contains true

  def isStartableOnAgent(position: Position, agentRefPath: AgentRefPath): Boolean =
    isStartableOnAgent(instruction(position), agentRefPath)

  private def isStartableOnAgent(instruction: Instruction, agentRefPath: AgentRefPath): Boolean =
    instruction match {
      case o: Fork => o.isStartableOnAgent(agentRefPath)
      case o: Execute.Anonymous => o.job.isExecutableOnAgent(agentRefPath)
      case o: Execute.Named => findJob(o.name) exists (_ isExecutableOnAgent agentRefPath)
      case _ => false
    }

  private[workflow] def isStartableOnAgent(agentRefPath: AgentRefPath): Boolean =
    checkedWorkflowJob(Position(0)) exists (_ isExecutableOnAgent agentRefPath)

  //def determinedExecutingAgent(position: Position): Option[AgentRefPath] =
  //  executingAgents(position) match {
  //    case a if a.size <= 1 => a.headOption
  //    case _ => None
  //  }
  //
  //private[workflow] def determinedExecutingAgent: Option[AgentRefPath] =
  //  determinedExecutingAgent(Position(0))
  //
  //def executingAgents(position: Position): Set[AgentRefPath] =
  //  instruction(position) match {
  //    case _: Execute =>
  //      checkedWorkflowJob(Position(0)).toOption.map(_.agentRefPath).toSet
  //
  //    case fork: Fork =>
  //      fork.startAgents
  //
  //    case _ => Set.empty
  //  }

  //private[workflow] def executingAgents: Set[AgentRefPath] =
  //  executingAgents(Position(0))

  def isDefinedAt(position: Position): Boolean =
    position match {
      case Position(Nil, nr) => isDefinedAt(nr)
      case Position(BranchPath.Segment(nr, branchId) :: tail, tailNr) =>
        instruction(nr).workflow(branchId) exists (_ isDefinedAt Position(tail, tailNr))
    }

  private def isDefinedAt(nr: InstructionNr): Boolean =
    labeledInstructions.indices isDefinedAt nr.number

  /** Searches a job bottom-up (from nested to root workflow).
    */
  @tailrec
  def findJob(name: WorkflowJob.Name): Checked[WorkflowJob] =
    nameToJob.get(name) match {
      case Some(job) => Valid(job)
      case None =>
        outer match {
          case None => Invalid(Problem(s"Unknown job '$name'"))
          case Some(o) => o.findJob(name)
        }
    }

  /** Looks up a job top-down. */
  def jobKey(branchPath: BranchPath, name: WorkflowJob.Name): Checked[JobKey] =
    jobKey(WorkflowBranchPath(id, branchPath), name)

  private def jobKey(workflowBranchPath: WorkflowBranchPath, name: WorkflowJob.Name): Checked[JobKey] =
    nestedWorkflow(workflowBranchPath.branchPath) flatMap (w =>
      if (w.nameToJob contains name)
        Valid(JobKey(workflowBranchPath, name))
      else
        workflowBranchPath.branchPath match {
          case Nil =>
            Invalid(Problem(s"Unknown job '$name'"))
          case branchPath =>
            jobKey(workflowBranchPath.copy(branchPath = branchPath.dropChild), name)
        })

  def keyToJob: Map[JobKey, WorkflowJob] =
    flattenedWorkflows flatMap { case (p, w) =>
      val workflowBranchPath = WorkflowBranchPath(id, p)
      val namedJobKeys = w.nameToJob.toVector.map { case (k, v) => JobKey.Named(workflowBranchPath, k) -> v }
      val anonymousJobKeys = w.numberedInstructions.collect {
        case (nr, _ @: (ex: Execute.Anonymous)) => JobKey.Anonymous(workflowBranchPath / nr) -> ex.job
      }
      namedJobKeys ++ anonymousJobKeys
    }

  def checkedWorkflowJob(position: Position): Checked[WorkflowJob] =
    checkedExecute(position) flatMap {
      case o: Execute.Anonymous => Valid(o.job)
      case o: Execute.Named => nestedWorkflow(position.branchPath) flatMap (_.findJob(o.name))
    }

  def checkedExecute(position: Position): Checked[Execute] =
    instruction(position) match {
      case o: Execute => Valid(o)
      case o => Invalid(Problem(s"Expected 'Execute' at workflow position $position (not: ${o.getClass.simpleScalaName})"))
    }

  /** Find catch instruction and return position of the first instruction. */
  def findCatchPosition(position: Position): Option[Position] =
    position.splitBranchAndNr flatMap { case (parent, branchId, _) =>
      instruction(parent).toCatchBranchId(branchId) match {
        case None => findCatchPosition(parent)
        case Some(catchBranchId) => Some(parent / catchBranchId % 0)
      }
    }

  def instruction(position: Position): Instruction =
    position match {
      case Position(Nil, nr) =>
        instruction(nr)

      case Position(BranchPath.Segment(nr, branchId) :: tail, tailNr) =>
        instruction(nr).workflow(branchId) map (_.instruction(Position(tail, tailNr))) getOrElse Gap
    }

  def instruction(nr: InstructionNr): Instruction =
    if (instructions.indices contains nr.number)
      instructions(nr.number)
    else
      Gap

  def labeledInstruction(position: Position): Instruction.Labeled =
    nestedWorkflow(position.branchPath).orThrow
      .labeledInstructions.get(position.nr.number)
      .getOrElse(throw new IllegalArgumentException(s"Unknown workflow position $position"))

  def withoutSource = copy(source = None)

  override def toString = (if (path != WorkflowPath.Anonymous) s"$id " else "") +
    s"{ ${labeledInstructions.mkString("; ")} ${nameToJob.map { case (k, v) => s"define job $k { $v }" }.mkString(" ")} }"
}

object Workflow extends FileBased.Companion[Workflow] {
  type ThisFileBased = Workflow
  type Path = WorkflowPath

  implicit val fileBasedsOverview = WorkflowsOverview
  val typedPathCompanion = WorkflowPath
  val empty = Workflow(WorkflowPath.NoId, Vector.empty)

  /** Test only. */
  def single(
    labeledInstruction: Instruction.Labeled,
    nameToJob: Map[WorkflowJob.Name, WorkflowJob] = Map.empty,
    source: Option[String] = None,
    outer: Option[Workflow] = None)
  : Workflow =
    anonymous(Vector(labeledInstruction), nameToJob, source, outer)

  def anonymous(
    labeledInstructions: IndexedSeq[Instruction.Labeled],
    nameToJob: Map[WorkflowJob.Name, WorkflowJob] = Map.empty,
    source: Option[String] = None,
    outer: Option[Workflow] = None)
  : Workflow =
    apply(WorkflowPath.NoId, labeledInstructions, nameToJob, source, outer)

  /** Test only. */
  def apply(
    id: WorkflowId,
    labeledInstructions: IndexedSeq[Instruction.Labeled],
    nameToJob: Map[WorkflowJob.Name, WorkflowJob] = Map.empty,
    source: Option[String] = None,
    outer: Option[Workflow] = None)
  : Workflow =
    checkedSub(id, labeledInstructions, nameToJob, source, outer).orThrow

  /** Checks a subworkflow.
    * Use `completelyChecked` to check a whole workflow incldung subworkfows. */
  def checkedSub(
    id: WorkflowId,
    labeledInstructions: IndexedSeq[Instruction.Labeled],
    nameToJob: Map[WorkflowJob.Name, WorkflowJob] = Map.empty,
    source: Option[String] = None,
    outer: Option[Workflow] = None)
  : Checked[Workflow] =
    new Workflow(
      id,
      labeledInstructions ++ !isCorrectlyEnded(labeledInstructions) ? (() @: ImplicitEnd),
      nameToJob,
      source,
      outer)
    .checked

  /** Checks a complete workflow including subworkflows. */
  def completelyChecked(
    id: WorkflowId,
    labeledInstructions: IndexedSeq[Instruction.Labeled],
    nameToJob: Map[WorkflowJob.Name, WorkflowJob] = Map.empty,
    source: Option[String] = None,
    outer: Option[Workflow] = None)
  : Checked[Workflow] =
    new Workflow(
      id,
      labeledInstructions ++ !isCorrectlyEnded(labeledInstructions) ? (() @: ImplicitEnd),
      nameToJob,
      source,
      outer)
    .completelyChecked

  def of(instructions: Instruction.Labeled*): Workflow =
    if (instructions.isEmpty)
      empty
    else
      of(WorkflowPath.NoId, instructions: _*)

  def of(id: WorkflowId, instructions: Instruction.Labeled*): Workflow =
    Workflow(id, instructions.toVector)

  def isCorrectlyEnded(labeledInstructions: IndexedSeq[Instruction.Labeled]): Boolean =
    labeledInstructions.nonEmpty &&
      (labeledInstructions.last.instruction.isInstanceOf[End] ||
       labeledInstructions.last.instruction.isInstanceOf[Goto])

  implicit val jsonEncoder: ObjectEncoder[Workflow] = {
    case Workflow(id, instructions, namedJobs, source, _) =>
      id.asJsonObject ++
        JsonObject(
          "instructions" -> instructions.reverse.dropWhile(_ == () @: ImplicitEnd).reverse.asJson,
          "jobs" -> emptyToNone(namedJobs).asJson,
          "source" -> source.asJson)
  }

  implicit val jsonDecoder: Decoder[Workflow] =
    // TODO Differentiate between CompleteWorkflow.completelyChecked and Subworkflow. completeChecked should not be left to the caller.
    cursor => for {
      id <- cursor.value.as[WorkflowId]
      instructions <- cursor.get[IndexedSeq[Instruction.Labeled]]("instructions")
      namedJobs <- cursor.get[Option[Map[WorkflowJob.Name, WorkflowJob]]]("jobs") map (_ getOrElse Map.empty)
      source <- cursor.get[Option[String]]("source")
      workflow <- Workflow.checkedSub(id, instructions, namedJobs, source).toDecoderResult
    } yield workflow

  // TODO Separate plain RawWorkflow, TopWorkflow and Subworkflow
  val topJsonDecoder: Decoder[Workflow] =
    cursor => jsonDecoder.decodeJson(cursor.value) flatMap (_.completelyChecked.toDecoderResult)
}
