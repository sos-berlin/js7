package js7.data.workflow

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, JsonObject}
import js7.base.circeutils.CirceUtils._
import js7.base.problem.Checked._
import js7.base.problem.Problems.UnknownKeyProblem
import js7.base.problem.{Checked, Problem}
import js7.base.utils.Assertions.assertThat
import js7.base.utils.Collections.emptyToNone
import js7.base.utils.Collections.implicits.{RichIndexedSeq, RichPairTraversable}
import js7.base.utils.ScalaUtils.reuseIfEqual
import js7.base.utils.ScalaUtils.syntax._
import js7.base.utils.typeclasses.IsEmpty.syntax._
import js7.data.agent.AgentId
import js7.data.item.{VersionedItem, VersionedItemId}
import js7.data.job.JobKey
import js7.data.value.expression.PositionSearch
import js7.data.workflow.Instruction.{@:, Labeled}
import js7.data.workflow.Workflow.isCorrectlyEnded
import js7.data.workflow.instructions.Instructions.jsonCodec
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.instructions.{End, Execute, Fork, Gap, Goto, If, IfFailedGoto, ImplicitEnd, Retry, TryInstruction}
import js7.data.workflow.position.{BranchPath, InstructionNr, Position, WorkflowBranchPath, WorkflowPosition}
import scala.annotation.tailrec

/**
  * @author Joacim Zschimmer
  */
final case class Workflow private(
  id: WorkflowId,
  rawLabeledInstructions: IndexedSeq[Instruction.Labeled],
  nameToJob: Map[WorkflowJob.Name, WorkflowJob],
  orderRequirements: OrderRequirements,
  source: Option[String],
  outer: Option[Workflow])
extends VersionedItem
{
  override def equals(o: Any) = o match {
    case o: Workflow =>
      id == o.id &&
      rawLabeledInstructions == o.rawLabeledInstructions &&
      nameToJob == o.nameToJob &&
      orderRequirements == o.orderRequirements &&
      source == o.source
      // Ignore `outer`
    case _ => false
  }

  assertThat(isCorrectlyEnded(rawLabeledInstructions), "Missing implicit end instruction")

  type Self = Workflow

  val companion = Workflow

  val labeledInstructions = rawLabeledInstructions.map(o => o.copy(
    instruction = o.instruction.adopt(this)))
  val instructions: IndexedSeq[Instruction] = labeledInstructions.map(_.instruction)
  private val labelToNumber: Map[Label, InstructionNr] =
    numberedInstructions.flatMap { case (nr, Instruction.Labeled(maybeLabel, _, _)) => maybeLabel.map(_ -> nr) }
      .uniqueToMap(labels => throw new IllegalArgumentException(s"Duplicate labels in Workflow: ${labels mkString ","}"))

  def withId(id: VersionedItemId[WorkflowPath]) = reuseIfEqual(this, copy(id = id))

  private def checked: Checked[Workflow] = {
    val problems = labeledInstructions.map (_.instruction).collect {
      case jump: JumpInstruction if !labelToNumber.contains(jump.to) =>
        Problem.pure(s"Unknown label '${jump.to}'")
    }
    if (problems.nonEmpty)
      Left(Problem.Combined(problems))
    else
      Right(this)
  }

  /** Checks a complete workflow including subworkflows using jobs in its outer workflows. */
  def completelyChecked: Checked[Workflow] = {
    val chk = flattenedWorkflows.map(_.checked) ++
      checkJobReferences ++
      checkRetry() ++
      checkLabels
    val problems = chk collect { case Left(problem) => problem }
    if (problems.nonEmpty)
      Left(Problem.Combined(problems))
    else
      Right(this)
  }

  def withPositions(branchPath: BranchPath): Workflow =
    copy(rawLabeledInstructions =
      rawLabeledInstructions.zipWithIndex map { case (labeled, i) =>
        val position = branchPath % InstructionNr(i)
        labeled.copy(
          maybePosition = Some(position),
          instruction = labeled.instruction withPositions position)
      })

  private def checkJobReferences: Seq[Checked[Unit]] =
    flattenedInstructions.collect {
      case (position, _ @: (o: Execute.Named)) =>
        nestedWorkflow(position.branchPath)
          .flatMap(_.findJob(o.name).map(_ => ()))
    }

  private def checkRetry(inCatch: Boolean = false): Seq[Checked[Unit]] =
    instructions flatMap {
      case _: Retry =>
        !inCatch thenList Left(Problem("Statement 'retry' is only allowed in a catch block"))
      case instr: If =>
        instr.workflows.flatMap(_.checkRetry(inCatch))
      case instr: TryInstruction =>
        Vector(instr.tryWorkflow.checkRetry(inCatch), instr.catchWorkflow.checkRetry(true)).flatten
      case instr =>
        instr.workflows.flatMap(_.checkRetry())
    }

  private def checkLabels: Iterable[Checked[Unit]] =
    flattenedInstructions.collect {
      case (position, Some(label) @: _) => (label, position)
    } .groupBy(_._1)
      .view
      .filter(_._2.lengthIs > 1)
      .mapValues(_.map(_._2))
      .map { case (label, positions) =>
        Left(Problem(s"Label '${label.string}' is duplicated at positions " + positions.mkString(", ")))
      }

  def positionMatchesSearch(position: Position, search: PositionSearch): Boolean =
    positionMatchesSearchNormalized(position.normalized, search)

  private def positionMatchesSearchNormalized(position: Position, search: PositionSearch): Boolean =
    search match {
      case PositionSearch.ByPrefix(name) =>
        labelToPosition(Label(name)) == Right(position) ||
          positionExecutesJob(position, WorkflowJob.Name(name))

      case PositionSearch.ByLabel(label) =>
        labelToPosition(label) == Right(position)

      case PositionSearch.ByWorkflowJob(jobName) =>
        positionExecutesJob(position, jobName)
    }

  private def positionExecutesJob(position: Position, jobName: WorkflowJob.Name) =
    instruction(position) match {
      case execute: Execute.Named => execute.name == jobName
      case _ => false
    }

  //def searchPositions(search: PositionSearch): Checked[Set[Position]] =
  //  search match {
  //    case PositionSearch.LastOccurredByPrefix(name) =>
  //      val checkedLabelPositions: Checked[List[Position]] = labelToPosition(Label(name)).map(_ :: Nil)
  //      val checkedJobPositions = jobNameToPositions(WorkflowJob.Name(name))
  //      (checkedLabelPositions, checkedJobPositions).mapN((a, b) => (a ++ b).toSet)
  //
  //    case PositionSearch.ByLabel(label) =>
  //      labelToPosition(label).map(o => Set.apply(o))
  //
  //    case PositionSearch.LastExecutedJob(jobName) =>
  //      val positions = flattenedInstructions flatMap {
  //        case (position, Labeled(_, execute: Execute.Named)) if execute.name == jobName => Some(position)
  //        case _ => None
  //      }
  //      if (positions.isEmpty) Left(Problem(s"Workflow does not execute a job '${jobName.string}'"))
  //      else Right(positions.toSet)
  //  }

  def labelToPosition(branchPath: BranchPath, label: Label): Checked[Position] =
    for {
      workflow <- nestedWorkflow(branchPath)
      nr <- workflow.labelToNumber.get(label).toChecked(UnknownKeyProblem("Label", label.string))
    } yield branchPath % nr

  def labelToPosition(label: Label): Checked[Position] =
    flattenedInstructions.find(_._2.maybeLabel contains label)
      .map(_._1)
      .toChecked(UnknownKeyProblem("Label", label.toString))

  def jobNameToPositions(name: WorkflowJob.Name): Checked[Seq[Position]] =
    findJob(name).map(_ => flattenedInstructions collect {
      case (pos, Labeled(_, ex: Execute.Named, _)) if ex.name == name => pos
    })

  def lastWorkflowPosition: WorkflowPosition =
    id /: Position(lastNr)

  def lastNr: InstructionNr =
    instructions.length - 1

  def anonymousJobKey(workflowPosition: WorkflowPosition): JobKey.Anonymous =
    JobKey.Anonymous(workflowPosition.normalized)

  private def flattenedWorkflows: Seq[Workflow] =
    flattenedWorkflowsOf(Nil).map(_._2)

  private[workflow] def flattenedBranchToWorkflow: Map[BranchPath, Workflow] =
    flattenedWorkflowsOf(Nil).toMap

  private[workflow] def flattenedWorkflowsOf(parents: BranchPath): List[(BranchPath, Workflow)] =
    (parents -> this) :: nestedWorkflows(parents)

  private def nestedWorkflows(parents: BranchPath): List[(BranchPath, Workflow)] =
    numberedInstructions.toList flatMap {
      case (nr, labeled) => labeled.instruction.flattenedWorkflows(parents % nr)
    }

  private[workflow] def nestedWorkflow(branchPath: BranchPath): Checked[Workflow] =
    branchPath match {
      case BranchPath.PositionAndBranchId(position, branchId) => instruction(position).workflow(branchId)
      case _ => Right(this)
    }

  def flattenedInstructions: Seq[(Position, Instruction.Labeled)] =
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
        case Seq(_ @: (jmp: JumpInstruction), maybeLabel @: _) if maybeLabel contains jmp.to => Nil
        case Seq(_ @: IfFailedGoto(errorTo, _), _ @: Goto(to, _)) if errorTo == to => Nil
        case Seq(a, _) => a :: Nil
        case Seq(_) => Nil  // Unused code in contrast to sliding's documentation?
      }.toVector ++
        rawLabeledInstructions.lastOption)

  private[workflow] def isPartiallyExecutableOnAgent(agentId: AgentId): Boolean =
    labeledInstructions.map(_.instruction) collect {
      case o: Execute.Anonymous => o.job isExecutableOnAgent agentId
      case o: Execute.Named => findJob(o.name) exists (_ isExecutableOnAgent agentId)
      case o: Fork => o isPartiallyExecutableOnAgent agentId
    } contains true

  def isStartableOnAgent(position: Position, agentId: AgentId): Boolean =
    isStartableOnAgent(instruction(position), agentId)

  private def isStartableOnAgent(instruction: Instruction, agentId: AgentId): Boolean =
    instruction match {
      case o: Fork => o.isStartableOnAgent(agentId)
      case o: Execute.Anonymous => o.job.isExecutableOnAgent(agentId)
      case o: Execute.Named => findJob(o.name) exists (_ isExecutableOnAgent agentId)
      case _ => false
    }

  private[workflow] def isStartableOnAgent(agentId: AgentId): Boolean =
    checkedWorkflowJob(Position(0)) exists (_ isExecutableOnAgent agentId)

  //def determinedExecutingAgent(position: Position): Option[AgentId] =
  //  executingAgents(position) match {
  //    case a if a.size <= 1 => a.headOption
  //    case _ => None
  //  }
  //
  //private[workflow] def determinedExecutingAgent: Option[AgentId] =
  //  determinedExecutingAgent(Position(0))
  //
  //def executingAgents(position: Position): Set[AgentId] =
  //  instruction(position) match {
  //    case _: Execute =>
  //      checkedWorkflowJob(Position(0)).toOption.map(_.agentId).toSet
  //
  //    case fork: Fork =>
  //      fork.startAgents
  //
  //    case _ => Set.empty
  //  }

  //private[workflow] def executingAgents: Set[AgentId] =
  //  executingAgents(Position(0))

  def isDefinedAt(position: Position): Boolean =
    position match {
      case Position(Nil, nr) => isDefinedAt(nr)
      case Position(BranchPath.Segment(nr, branchId) :: tail, tailNr) =>
        instruction(nr).workflow(branchId).exists(_ isDefinedAt Position(tail, tailNr))
    }

  private def isDefinedAt(nr: InstructionNr): Boolean =
    labeledInstructions.indices isDefinedAt nr.number

  /** Searches a job bottom-up (from nested to root workflow).
    */
  @tailrec
  def findJob(name: WorkflowJob.Name): Checked[WorkflowJob] =
    nameToJob.get(name) match {
      case Some(job) => Right(job)
      case None =>
        outer match {
          case None => Left(Problem(s"Unknown job name '$name'"))
          case Some(o) => o.findJob(name)
        }
    }

  /** Looks up a job top-down. */
  def jobKey(branchPath: BranchPath, name: WorkflowJob.Name): Checked[JobKey] =
    jobKey(WorkflowBranchPath(id, branchPath), name)

  private def jobKey(workflowBranchPath: WorkflowBranchPath, name: WorkflowJob.Name): Checked[JobKey] =
    nestedWorkflow(workflowBranchPath.branchPath).flatMap(w =>
      if (w.nameToJob contains name)
        Right(JobKey(workflowBranchPath, name))
      else
        workflowBranchPath.branchPath match {
          case Nil =>
            Left(Problem(s"Unknown job name '$name'"))
          case branchPath =>
            jobKey(workflowBranchPath.copy(branchPath = branchPath.dropChild), name)
        })

  def keyToJob: Map[JobKey, WorkflowJob] =
    flattenedBranchToWorkflow flatMap { case (branchPath, workflow) =>
      val workflowBranchPath = WorkflowBranchPath(id, branchPath)
      val namedJobKeys = workflow.nameToJob.toVector.map { case (k, v) => JobKey.Named(workflowBranchPath, k) -> v }
      val anonymousJobKeys = workflow.numberedInstructions.collect {
        case (nr, _ @: (ex: Execute.Anonymous)) => JobKey.Anonymous(workflowBranchPath / nr) -> ex.job
      }
      namedJobKeys ++ anonymousJobKeys: Seq[(JobKey, WorkflowJob)]/*for IntelliJ*/
    }

  def checkedWorkflowJob(position: Position): Checked[WorkflowJob] =
    checkedExecute(position) flatMap {
      case o: Execute.Anonymous => Right(o.job)
      case o: Execute.Named => nestedWorkflow(position.branchPath).flatMap(_.findJob(o.name))
    }

  def checkedExecute(position: Position): Checked[Execute] =
    instruction(position) match {
      case o: Execute => Right(o)
      case o => Left(Problem(s"Expected 'Execute' statement at workflow position $position (not: ${o.getClass.simpleScalaName})"))
    }

  def isMoveable(from: Position, to: Position): Boolean =
    isDefinedAt(from) && isDefinedAt(to) && isMoveable(from.branchPath, to.branchPath)

  private def isMoveable(from: BranchPath, to: BranchPath): Boolean = {
    val prefix = BranchPath.commonBranchPath(from, to)
    !from.drop(prefix.length).exists(_.branchId.isFork) &&
      !to.drop(prefix.length).exists(_.branchId.isFork)
  }

  def instruction(position: Position): Instruction =
    position match {
      case Position(Nil, nr) =>
        instruction(nr)

      case Position(BranchPath.Segment(nr, branchId) :: tail, tailNr) =>
        instruction(nr).workflow(branchId.normalized)
          .map(_.instruction(Position(tail, tailNr)))
          .getOrElse(Gap.empty)
    }

  def instruction(nr: InstructionNr): Instruction =
    if (instructions.indices contains nr.number)
      instructions(nr.number)
    else
      Gap.empty

  def checkedPosition(position: Position): Checked[Position] =
    labeledInstruction(position).map(_ => position)

  def labeledInstruction(position: Position): Checked[Instruction.Labeled] =
    for {
      workflow <- nestedWorkflow(position.branchPath)
      instr <- workflow.labeledInstructions.get(position.nr.number)
        .toRight(Problem(s"Unknown position $position in workflow '$id'"))
    } yield instr

  def withoutSource: Workflow =
    copy(source = None).withoutSourcePos

  def withoutSourcePos: Workflow = reuseIfEqual(this,
    copy(rawLabeledInstructions = rawLabeledInstructions
      .map(labeled => labeled.copy(
        instruction = labeled.instruction.withoutSourcePos))))

  override def toString = ((path != WorkflowPath.Anonymous) ?? s"$id ") +
    s"{ ${labeledInstructions.mkString("; ")} ${nameToJob.map { case (k, v) => s"define job $k { $v }" }.mkString(" ")} }"
}

object Workflow extends VersionedItem.Companion[Workflow]
{
  type ThisItem = Workflow
  type Path = WorkflowPath

  implicit val itemsOverview = WorkflowsOverview
  val itemPathCompanion = WorkflowPath
  val empty = Workflow(WorkflowPath.NoId, Vector.empty)

  def anonymous(
    labeledInstructions: IndexedSeq[Instruction.Labeled],
    nameToJob: Map[WorkflowJob.Name, WorkflowJob] = Map.empty,
    source: Option[String] = None,
    outer: Option[Workflow] = None)
  : Workflow =
    apply(WorkflowPath.NoId, labeledInstructions, nameToJob, OrderRequirements.empty, source, outer)

  /** Test only. */
  def apply(
    id: WorkflowId,
    labeledInstructions: IndexedSeq[Instruction.Labeled],
    nameToJob: Map[WorkflowJob.Name, WorkflowJob] = Map.empty,
    orderRequirements: OrderRequirements = OrderRequirements.empty,
    source: Option[String] = None,
    outer: Option[Workflow] = None)
  : Workflow =
    checkedSub(id, labeledInstructions, nameToJob, orderRequirements, source, outer).orThrow

  /** Checks a subworkflow.
    * Use `completelyChecked` to check a whole workflow incldung subworkfows. */
  def checkedSub(
    id: WorkflowId,
    labeledInstructions: IndexedSeq[Instruction.Labeled],
    nameToJob: Map[WorkflowJob.Name, WorkflowJob] = Map.empty,
    orderRequirements: OrderRequirements = OrderRequirements.empty,
    source: Option[String] = None,
    outer: Option[Workflow] = None)
  : Checked[Workflow] =
    new Workflow(
      id,
      labeledInstructions ++ !isCorrectlyEnded(labeledInstructions) ? (() @: ImplicitEnd()),
      nameToJob,
      orderRequirements,
      source,
      outer
    ).checked

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
      labeledInstructions ++ !isCorrectlyEnded(labeledInstructions) ? (() @: ImplicitEnd()),
      nameToJob,
      OrderRequirements.empty,
      source,
      outer
    ).completelyChecked

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

  implicit val jsonEncoder: Encoder.AsObject[Workflow] = {
    case Workflow(id, instructions, namedJobs, orderRequirements, source, _) =>
      id.asJsonObject ++
        JsonObject(
          "orderRequirements" -> orderRequirements.??.asJson,
          "instructions" -> instructions
            .dropLastWhile(labeled => labeled.instruction == ImplicitEnd() && labeled.maybePosition.isEmpty)
            .asJson,
          "jobs" -> emptyToNone(namedJobs).asJson,
          "source" -> source.asJson)
  }

  implicit val jsonDecoder: Decoder[Workflow] =
    // TODO Differentiate between CompleteWorkflow.completelyChecked and Subworkflow. completeChecked should not be left to the caller.
    cursor => for {
      id <- cursor.value.as[WorkflowId]
      instructions <- cursor.get[IndexedSeq[Instruction.Labeled]]("instructions")
      namedJobs <- cursor.getOrElse[Map[WorkflowJob.Name, WorkflowJob]]("jobs")(Map.empty)
      orderRequirements <- cursor.getOrElse[OrderRequirements]("orderRequirements")(OrderRequirements.empty)
      source <- cursor.get[Option[String]]("source")
      workflow <- Workflow.checkedSub(id, instructions, namedJobs, orderRequirements, source)
        .toDecoderResult(cursor.history)
    } yield workflow

  // TODO Separate plain RawWorkflow, TopWorkflow and Subworkflow
  val topJsonDecoder: Decoder[Workflow] =
    cursor => jsonDecoder.decodeJson(cursor.value).flatMap(_.completelyChecked.toDecoderResult(cursor.history))
}
