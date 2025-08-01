package js7.data.workflow

import cats.syntax.foldable.*
import cats.syntax.traverse.*
import io.circe.syntax.EncoderOps
import io.circe.{Codec, Decoder, Encoder, JsonObject}
import js7.base.circeutils.CirceUtils.*
import js7.base.circeutils.typed.Subtype
import js7.base.problem.Checked.*
import js7.base.problem.Problems.UnknownKeyProblem
import js7.base.problem.{Checked, Problem}
import js7.base.time.Timezone
import js7.base.utils.Assertions.assertThat
import js7.base.utils.Collections.implicits.{RichIndexedSeq, RichPairTraversable}
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.ScalaUtils.{implicitClass, reuseIfEqual}
import js7.base.utils.typeclasses.IsEmpty.syntax.*
import js7.data.agent.AgentPath
import js7.data.calendar.CalendarPath
import js7.data.item.{InventoryItemPath, SignableSimpleItemPath, SimpleItemPath, TrivialItemState, UnsignedSimpleItemPath, VersionedItem, VersionedItemId, VersionedItemPath}
import js7.data.job.{JobKey, JobResourcePath}
import js7.data.order.Order
import js7.data.subagent.SubagentBundleId
import js7.data.value.expression.{Expression, PositionSearch, Scope}
import js7.data.workflow.Instruction.{@:, Labeled}
import js7.data.workflow.Workflow.isCorrectlyEnded
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.instructions.{Break, Cycle, End, Execute, Fork, ForkInstruction, Gap, If, ImplicitEnd, Instructions, LockInstruction, NoticeInstruction, Retry, TryInstruction}
import js7.data.workflow.position.*
import js7.data.workflow.position.BranchPath.syntax.*
import scala.annotation.tailrec
import scala.collection.View
import scala.reflect.ClassTag

/**
  * @author Joacim Zschimmer
  */
final case class Workflow(
  id: WorkflowId,
  rawLabeledInstructions: IndexedSeq[Instruction.Labeled],
  nameToJob: Map[WorkflowJob.Name, WorkflowJob],
  orderPreparation: OrderPreparation,
  timeZone: Timezone,
  jobResourcePaths: Seq[JobResourcePath],
  calendarPath: Option[CalendarPath],
  result: Option[Map[String, Expression]],
  source: Option[String],
  outer: Option[Workflow])
extends VersionedItem, TrivialItemState[Workflow]:

  override def equals(o: Any): Boolean = o match
    case o: Workflow =>
      (id == o.id
        && rawLabeledInstructions == o.rawLabeledInstructions
        && nameToJob == o.nameToJob
        && orderPreparation == o.orderPreparation
        && timeZone == o.timeZone
        && jobResourcePaths == o.jobResourcePaths
        && calendarPath == o.calendarPath
        && result == o.result
        && source == o.source)
      // Ignore `outer`
    case _ => false

  assertThat(isCorrectlyEnded(rawLabeledInstructions), "Missing implicit end instruction")

  val companion: Workflow.type = Workflow

  override def path: WorkflowPath = super.path

  val labeledInstructions: IndexedSeq[Labeled] = rawLabeledInstructions
    .map(o => o.copy(
      instruction = o.instruction.adopt(this)))
  val instructions: IndexedSeq[Instruction] = labeledInstructions.map(_.instruction)
  private val labelToNumber: Map[Label, InstructionNr] =
    numberedInstructions
      .flatMap { case (nr, Instruction.Labeled(maybeLabel, _, _)) => maybeLabel.map(_ -> nr) }
      .uniqueToMap(labels => throw new IllegalArgumentException(
        s"Duplicate labels in Workflow: ${labels mkString ","}"))

  def withId(id: VersionedItemId[WorkflowPath]): Workflow =
    reuseIfEqual(this, copy(id = id))

  /** Checks a complete workflow including subworkflows using jobs in its outer workflows. */
  def completelyChecked: Checked[Workflow] =
    val chk =
      checkJobReferences ++
      checkJobs ++
      checkRetry() ++
      checkLabels ++
      checkCalendar ++
      checkBreak()
    val problems = chk.collect { case Left(problem) => problem }.toVector
    if problems.nonEmpty then
      Left(Problem.Combined(problems))
    else
      Right(this)

  def withPositions(branchPath: BranchPath): Workflow =
    copy(rawLabeledInstructions =
      rawLabeledInstructions.zipWithIndex.map: (labeled, i) =>
        val position = branchPath % InstructionNr(i)
        labeled.copy(
          maybePosition = Some(position),
          instruction = labeled.instruction.withPositions(position)))

  private def checkJobReferences: View[Checked[Unit]] =
    flattenedInstructions
      .collect:
        case (position, _ @: (o: Execute.Named)) =>
          nestedWorkflow(position.branchPath)
            .flatMap(_.findJob(o.name).map(_ => ()))

  private def checkJobs: View[Checked[Unit]] =
    workflowJobs.map(_.checked)

  private def checkRetry(inCatch: Boolean = false): Seq[Checked[Unit]] =
    instructions flatMap:
      case _: Retry =>
        !inCatch thenList Left(Problem("Statement 'retry' is only allowed in a catch block"))
      case instr: If =>
        instr.workflows.flatMap(_.checkRetry(inCatch))
      case instr: TryInstruction =>
        Vector(instr.tryWorkflow.checkRetry(inCatch), instr.catchWorkflow.checkRetry(true)).flatten
      case instr =>
        instr.workflows.flatMap(_.checkRetry())

  private def checkLabels: View[Checked[Unit]] =
    flattenedInstructions
      .collect:
        case (position, Some(label) @: _) => (label, position)
      .toVector
      .groupBy(_._1)
      .view
      .filter(_._2.lengthIs > 1)
      .mapValues(_.map(_._2))
      .map { case (label, positions) =>
        Left(Problem(s"Label '${label.string}' is duplicated at positions " +
          positions.mkString(", ")))
      }

  private def checkCalendar: View[Checked[Unit]] =
    new View.Single(
      (!flattenedInstructions.exists(_._2.instruction.isInstanceOf[Cycle])
        || calendarPath.nonEmpty
      ) !! Problem("Cycle instruction requires calendarPath"))

  private def checkBreak(branchPath: BranchPath = Nil, inCycle: Boolean = false)
  : View[Checked[Unit]] =
    for (nr, Labeled(_, instr, _)) <- numberedInstructions yield
      for
        isInCycle <- instr
          .match
            case _: Cycle => Right(true)
            case _: Break if !inCycle =>
              Left(Problem.pure(s"Break instruction at ${branchPath % nr} without Cycle"))
            case _: Instruction.IsOrderBoundary => Right(false)
            case _ => Right(inCycle)
        _ <- instr
          .branchWorkflows
          .flatMap { case (branchId, w) =>
            w.checkBreak(branchPath % nr / branchId, isInCycle)
          }
          .sequence
          .map(_.combineAll)
      yield ()

  lazy val referencedAttachableUnsignedPaths
  : Vector[InventoryItemPath.AttachableToAgent & UnsignedSimpleItemPath] =
    referencedItemPaths
      .collect { case o: (InventoryItemPath.AttachableToAgent & UnsignedSimpleItemPath) => o }
      .toVector

  lazy val referencedAttachableToAgentSignablePaths
  : Vector[InventoryItemPath.AttachableToAgent & SignableSimpleItemPath] =
    referencedItemPaths
      .collect { case o: (InventoryItemPath.AttachableToAgent & SignableSimpleItemPath) => o }
      .toVector

  override lazy val referencedItemPaths: View[SimpleItemPath] =
    val referencedLockAndBoardPaths =
      flattenedInstructions.view
        .map(_._2.instruction)
        .collect:
          case instr: LockInstruction => instr.lockPaths
          case instr: NoticeInstruction => instr.referencedBoardPaths
        .flatten
        .toSet

    val referencedJobResourcePaths =
      jobResourcePaths.view
        .concat(workflowJobs.view.flatMap(_.referencedJobResourcePaths))
        .concat(orderPreparation.referencedJobResourcePaths)
        .toSet

    View.concat(
      referencedLockAndBoardPaths,
      referencedJobResourcePaths,
      calendarPath,
      referencedAgentPaths,
      knownSubagentBundleIds)

  private[workflow] def knownSubagentBundleIds: Set[SubagentBundleId] =
    workflowJobs.view.flatMap(_
      .subagentBundleId
      .flatMap(_
        .evalAsString(using Scope.empty)
        .flatMap(SubagentBundleId.checked)
        .toOption))
      .toSet

  def referencedAgentPaths: Set[AgentPath] =
    workflowJobs.map(_.agentPath).toSet

  private[workflow] def workflowJobs: View[WorkflowJob] =
    keyToJob.values.view

  def positionMatchesSearch(position: Position, search: PositionSearch): Boolean =
    positionMatchesSearchNormalized(position.normalized, search)

  private def positionMatchesSearchNormalized(position: Position, search: PositionSearch): Boolean =
    search match
      case PositionSearch.ByPrefix(name) =>
        labelToPosition(Label(name)) == Right(position) ||
          positionExecutesJob(position, WorkflowJob.Name(name))

      case PositionSearch.ByLabel(label) =>
        labelToPosition(label) == Right(position)

      case PositionSearch.ByWorkflowJob(jobName) =>
        positionExecutesJob(position, jobName)

  private def positionExecutesJob(position: Position, jobName: WorkflowJob.Name) =
    instruction(position) match
      case execute: Execute.Named => execute.name == jobName
      case _ => false

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
    for
      workflow <- nestedWorkflow(branchPath)
      nr <- workflow.labelToNumber.get(label).toChecked(UnknownKeyProblem("Label", label.string))
    yield branchPath % nr

  def labelToPosition(label: Label): Checked[Position] =
    flattenedInstructions
      .find(_._2.maybeLabel contains label)
      .map(_._1)
      .toChecked(UnknownKeyProblem("Label", label.toString))

  def positionOrLabelToPosition(position: PositionOrLabel): Checked[Position] =
    position match
      case position: Position => Right(position)
      case label: Label => labelToPosition(label)

  def isOrderAtStopPosition(order: Order[Order.State]): Boolean =
    !isOrderAtEndOfWorkflow(order) && (
      order.stopPositions.contains(order.position.normalized) ||
        order.stopPositions.exists(_.isInstanceOf[Label]) &&
          labeledInstruction(order.position)
            .toOption
            .flatMap(_.maybeLabel)
            .exists(order.stopPositions.contains))

  private def isOrderAtEndOfWorkflow(order: Order[Order.State]): Boolean =
    order.isInOutermostBlock && this.instruction(order.position).isInstanceOf[End]

  def lastWorkflowPosition: WorkflowPosition =
    id /: Position(lastNr)

  def lastNr: InstructionNr =
    instructions.length - 1

  private[workflow] def flattenedBranchToWorkflow: Map[BranchPath, Workflow] =
    flattenedWorkflowsOf(Nil).toMap

  private[workflow] def flattenedWorkflowsOf(parents: BranchPath): View[(BranchPath, Workflow)] =
    View(parents -> this) ++ nestedWorkflows(parents)

  private def nestedWorkflows(parents: BranchPath): View[(BranchPath, Workflow)] =
    numberedInstructions flatMap:
      case (nr, labeled) => labeled.instruction.flattenedWorkflows(parents % nr)

  def nestedWorkflow(branchPath: BranchPath): Checked[Workflow] =
    branchPath match
      case BranchPath.PositionAndBranchId(position, branchId) => instruction(position).workflow(branchId)
      case _ => Right(this)

  def flattenedInstructions: View[(Position, Instruction.Labeled)] =
    flattenedInstructions(Nil)

  private[workflow] def flattenedInstructions(branchPath: BranchPath)
  : View[(Position, Instruction.Labeled)] =
    numberedInstructions.flatMap:
      case (nr, labeled) =>
        View((branchPath % nr) -> labeled) ++
          labeled.instruction.flattenedInstructions(branchPath % nr)

  def numberedInstructions: View[(InstructionNr, Instruction.Labeled)] =
    labeledInstructions.view
      .zipWithIndex
      .map { case (s, i) => InstructionNr(i) -> s }

  def reduceForAgent(agentPath: AgentPath): Workflow =
    reuseIfEqual(this, copy(
      rawLabeledInstructions = labeledInstructions
        .map(labeled => labeled.copy(
          instruction = labeled.instruction
            .reduceForAgent(agentPath, this))),
      nameToJob = reuseIfEqual(nameToJob,
        nameToJob.filter(o => o._2.agentPath == agentPath))))

  private[workflow] def isVisibleForAgent(agentPath: AgentPath): Boolean =
    instructions.exists(_.isVisibleForAgent(agentPath, this))

  def isStartableOnAgent(position: Position, agentPath: AgentPath): Boolean =
    isStartableOnAgent(instruction(position), agentPath)

  private def isStartableOnAgent(instruction: Instruction, agentPath: AgentPath): Boolean =
    instruction match
      case o: Fork => o.isStartableOnAgent(agentPath)
      case o: Execute => o.isVisibleForAgent(agentPath, this)
      case _ => false

  private[workflow] def isStartableOnAgent(agentPath: AgentPath): Boolean =
    checkedWorkflowJob(Position(0)).exists(_.isExecutableOnAgent(agentPath))

  def agentPath(position: Position): Option[AgentPath] =
    instruction(position) match
      case execute: Execute =>
        checkedWorkflowJob(position).toOption.map(_.agentPath)

      case fork: ForkInstruction =>
        fork.agentPath

      case _ => None

  def isDefinedAt(position: Position): Boolean =
    position match
      case Position(Nil, nr) => isDefinedAt(nr)
      case Position(BranchPath.Segment(nr, branchId) :: tail, tailNr) =>
        instruction(nr).workflow(branchId).exists(_.isDefinedAt(Position(tail, tailNr)))

  private def isDefinedAt(nr: InstructionNr): Boolean =
    labeledInstructions.indices isDefinedAt nr.number

  /** Searches a job bottom-up (from nested to root workflow).
    */
  @tailrec
  def findJob(name: WorkflowJob.Name): Checked[WorkflowJob] =
    nameToJob.get(name) match
      case Some(job) => Right(job)
      case None =>
        outer match
          case None => Left(Problem(s"Unknown job name '$name'"))
          case Some(o) => o.findJob(name)

  def positionToJobKey(position: Position): Checked[JobKey] =
    checkedExecute(position).flatMap:
      case _: Execute.Anonymous => Right(anonymousJobKey(id /: position))
      case o: Execute.Named     => jobKey(position.branchPath, o.name)

  def maybePositionToJobKey(position: Position): Option[JobKey] =
    maybeExecuteInstr(position).flatMap:
      case _: Execute.Anonymous => Some(anonymousJobKey(id /: position))
      case o: Execute.Named     => jobKey(position.branchPath, o.name).toOption

  def anonymousJobKey(workflowPosition: WorkflowPosition): JobKey.Anonymous =
    JobKey.Anonymous(workflowPosition.normalized)

  /** Looks up a job top-down. */
  def jobKey(branchPath: BranchPath, name: WorkflowJob.Name): Checked[JobKey] =
    jobKey(WorkflowBranchPath(id, branchPath), name)

  private def jobKey(workflowBranchPath: WorkflowBranchPath, name: WorkflowJob.Name): Checked[JobKey] =
    nestedWorkflow(workflowBranchPath.branchPath).flatMap: w =>
      if w.nameToJob contains name then
        Right(JobKey(workflowBranchPath, name))
      else
        workflowBranchPath.branchPath match
          case Nil =>
            Left(Problem(s"Unknown job name '$name'"))
          case branchPath =>
            jobKey(workflowBranchPath.copy(branchPath = branchPath.dropChild), name)

  lazy val keyToJob: Map[JobKey, WorkflowJob] =
    flattenedBranchToWorkflow.flatMap { case (branchPath, workflow) =>
      val workflowBranchPath = WorkflowBranchPath(id, branchPath)
      val namedJobKeys = workflow.nameToJob.view
        .map { case (k, v) => JobKey.Named(workflowBranchPath, k) -> v }
      val anonymousJobKeys = workflow.numberedInstructions.view.collect:
        case (nr, _ @: (ex: Execute.Anonymous)) => JobKey.Anonymous(workflowBranchPath / nr) -> ex.job
      namedJobKeys ++ anonymousJobKeys
    }

  def checkedWorkflowJob(position: Position): Checked[WorkflowJob] =
    checkedExecute(position) flatMap:
      case o: Execute.Anonymous => Right(o.job)
      case o: Execute.Named => nestedWorkflow(position.branchPath).flatMap(_.findJob(o.name))

  def checkedExecute(position: Position): Checked[Execute] =
    instruction(position) match
      case o: Execute => Right(o)
      case o => Left(Problem(s"Expected 'Execute' statement at workflow position $position (not: ${o.getClass.simpleScalaName})"))

  def maybeExecuteInstr(position: Position): Option[Execute] =
    instruction(position) match
      case o: Execute => Some(o)
      case _ => None

  def reachablePositions(from: Position): Iterable[Position] =
    if !isDefinedAt(from) then
      Nil
    else
      flattenedInstructions.view
        .map(_._1)
        .filter(pos => isMoveable(from.branchPath, pos.branchPath))

  def isMoveable(from: Position, to: Position): Boolean =
    isDefinedAt(from) && isDefinedAt(to) && isMoveable(from.branchPath, to.branchPath)

  private def isMoveable(from: BranchPath, to: BranchPath): Boolean =
    from == to || {
      val prefix = BranchPath.commonBranchPath(from, to).length
      from.drop(prefix).forall(_.branchId.isNoMoveBoundary) &&
        to.drop(prefix).forall(_.branchId.isNoMoveBoundary)
    }

  def instruction(position: Position): Instruction =
    position match
      case Position(Nil, nr) =>
        instruction(nr)

      case Position(BranchPath.Segment(nr, branchId) :: tail, tailNr) =>
        instruction(nr).workflow(branchId.normalized)
          .map(_.instruction(Position(tail, tailNr)))
          .getOrElse(Gap.empty)

  def instruction(nr: InstructionNr): Instruction =
    if instructions.indices contains nr.number then
      instructions(nr.number)
    else
      Gap.empty

  def instruction_[A <: Instruction: ClassTag](position: Position)
  : Checked[A] =
    instruction(position) match
      case o if implicitClass[A].isAssignableFrom(o.getClass) =>
        Right(o.asInstanceOf[A])
      case o =>
        Left(Problem(s"'${Instructions.jsonCodec.classToName(implicitClass[A])}' Instruction " +
          s"is expected at position ${id /: position}, not: ${Instructions.jsonCodec.typeName(o)}"))

  def checkPosition(position: Position): Checked[Unit] =
    labeledInstruction(position).rightAs(())

  def labeledInstruction(position: Position): Checked[Instruction.Labeled] =
    for
      workflow <- nestedWorkflow(position.branchPath)
      instr <- workflow.labeledInstructions.get(position.nr.number)
        .toRight(Problem(s"Unknown position $position in $id"))
    yield instr

  def orderParameterList: OrderParameterList =
    orderPreparation.parameterList

  def withoutSource: Workflow =
    copy(source = None).withoutSourcePos

  def withoutSourcePos: Workflow = reuseIfEqual(this,
    copy(rawLabeledInstructions = rawLabeledInstructions
      .map(labeled => labeled.copy(
        instruction = labeled.instruction.withoutSourcePos))))

  override def toString = ((path != WorkflowPath.Anonymous) ?? s"$id ") +
    s"{${labeledInstructions.mkString("; ")} ${nameToJob.map { case (k, v) => s"define job $k { $v }" }.mkString(" ")} }"


object Workflow extends VersionedItem.Companion[Workflow], TrivialItemState.Companion[Workflow]:

  type Item = Workflow
  type Path = WorkflowPath

  val cls: Class[Workflow] = classOf[Workflow]
  val Path: VersionedItemPath.Companion[WorkflowPath] = WorkflowPath
  val empty: Workflow = Workflow(WorkflowPath.NoId, Vector.empty)

  def anonymous(
    labeledInstructions: Seq[Instruction.Labeled],
    nameToJob: Map[WorkflowJob.Name, WorkflowJob] = Map.empty,
    result: Option[Map[String, Expression]] = None,
    source: Option[String] = None,
    outer: Option[Workflow] = None)
  : Workflow =
    apply(WorkflowPath.NoId, labeledInstructions, nameToJob, OrderPreparation.default,
      Timezone.utc, result = result,
      source = source, outer = outer)

  /** Test only. */
  def apply(
    id: WorkflowId,
    instructions: Seq[Instruction.Labeled],
    nameToJob: Map[WorkflowJob.Name, WorkflowJob] = Map.empty,
    orderPreparation: OrderPreparation = OrderPreparation.default,
    timeZone: Timezone = Timezone.utc,
    jobResourcePaths: Seq[JobResourcePath] = Nil,
    calendarPath: Option[CalendarPath] = None,
    result: Option[Map[String, Expression]] = None,
    source: Option[String] = None,
    outer: Option[Workflow] = None)
  : Workflow =
    checkedSub(id, instructions.toIndexedSeq, nameToJob, orderPreparation,
      timeZone, jobResourcePaths, calendarPath, result, source, outer).orThrow

  /** Checks a subworkflow.
    * Use `completelyChecked` to check a whole workflow including subworkfows. */
  def checkedSub(
    id: WorkflowId,
    labeledInstructions: IndexedSeq[Instruction.Labeled],
    nameToJob: Map[WorkflowJob.Name, WorkflowJob] = Map.empty,
    orderPreparation: OrderPreparation = OrderPreparation.default,
    timeZone: Timezone = Timezone.utc,
    jobResourcePaths: Seq[JobResourcePath] = Nil,
    calendarPath: Option[CalendarPath] = None,
    result: Option[Map[String, Expression]] = None,
    source: Option[String] = None,
    outer: Option[Workflow] = None)
  : Checked[Workflow] =
    Right(new Workflow(
      id,
      labeledInstructions ++ !isCorrectlyEnded(labeledInstructions) ? (() @: ImplicitEnd()),
      nameToJob,
      orderPreparation,
      timeZone,
      jobResourcePaths,
      calendarPath,
      result,
      source,
      outer))

  def of(instructions: Instruction.Labeled*): Workflow =
    if instructions.isEmpty then
      empty
    else
      of(WorkflowPath.NoId, instructions*)

  def of(id: WorkflowId, instructions: Instruction.Labeled*): Workflow =
    Workflow(id, instructions.toVector, jobResourcePaths = Nil)

  def isCorrectlyEnded(labeledInstructions: IndexedSeq[Instruction.Labeled]): Boolean =
    labeledInstructions.nonEmpty &&
      labeledInstructions.last.instruction.isInstanceOf[End]

  override lazy val subtype: Subtype[Workflow] =
    Subtype(jsonEncoder, topJsonDecoder)

  implicit lazy val jsonCodec: Codec.AsObject[Workflow] =
    Codec.AsObject.from(jsonDecoder_, jsonEncoder_)

  private val jsonEncoder_ : Encoder.AsObject[Workflow] =
    case Workflow(id, instructions, namedJobs, orderPreparation, tz, jobResourcePaths,
    calendarPath, result, src, _) =>
      implicit val x: Encoder.AsObject[Instruction] = Instructions.jsonCodec
      id.asJsonObject ++
        JsonObject(
          "orderPreparation" -> orderPreparation.??.asJson,
          "timeZone" -> ((tz != Timezone.utc) ? tz).asJson,
          "jobResourcePaths" -> jobResourcePaths.??.asJson,
          "calendarPath" -> calendarPath.asJson,
          "instructions" -> instructions
            .dropLastWhile(labeled =>
              labeled.instruction == ImplicitEnd.empty && labeled.maybePosition.isEmpty)
            .asJson,
          "jobs" -> namedJobs.??.asJson,
          "result" -> result.asJson,
          "source" -> src.asJson)

  private val jsonDecoder_ : Decoder[Workflow] =
    // TODO Differentiate between CompleteWorkflow.completelyChecked and Subworkflow. completeChecked should not be left to the caller.
    cursor => {
      implicit val x: Decoder[Instruction] = Instructions.jsonCodec
      for
        id <- cursor.value.as[WorkflowId]
        orderPreparation <- cursor.getOrElse[OrderPreparation]("orderPreparation")(OrderPreparation.default)
        tz <- cursor.getOrElse[Timezone]("timeZone")(Timezone.utc)
        jobResourcePaths <- cursor.getOrElse[Seq[JobResourcePath]]("jobResourcePaths")(Nil)
        calendarPath <- cursor.get[Option[CalendarPath]]("calendarPath")
        instructions <- cursor.get[IndexedSeq[Instruction.Labeled]]("instructions")
        namedJobs <- cursor.getOrElse[Map[WorkflowJob.Name, WorkflowJob]]("jobs")(Map.empty)
        result <- cursor.get[Option[Map[String, Expression]]]("result")
        source <- cursor.get[Option[String]]("source")
        workflow <- Workflow.checkedSub(id, instructions, namedJobs, orderPreparation, tz,
          jobResourcePaths, calendarPath, result, source)
          .toDecoderResult(cursor.history)
      yield workflow
    }

  // TODO Separate plain RawWorkflow, TopWorkflow and Subworkflow
  val topJsonDecoder: Decoder[Workflow] =
    cursor => jsonDecoder.decodeJson(cursor.value).flatMap(_.completelyChecked.toDecoderResult(cursor.history))
