package js7.data.workflow.instructions

import io.circe.*
import io.circe.derivation.ConfiguredDecoder
import io.circe.generic.semiauto.deriveEncoder
import js7.base.problem.Checked
import js7.base.utils.ScalaUtils.reuseIfEqual
import js7.data.agent.AgentPath
import js7.data.source.SourcePos
import js7.data.value.expression.{ExprFunction, Expression}
import js7.data.workflow.position.{BranchId, Position}
import js7.data.workflow.{Instruction, Workflow}

/** Fork a list. */
final case class ForkList(
  children: Expression,
  childToId: ExprFunction,
  childToArguments: ExprFunction,
  workflow: Workflow,
  agentPath: Option[AgentPath] = None,
  joinIfFailed: Boolean = false,
  sourcePos: Option[SourcePos] = None)
extends ForkInstruction:

  def checked: Checked[ForkList] =
    for
      childToId <- childToId.restrict("childToId", minimum = 1, maximum = 2)
      childToArguments <- childToArguments.restrict("childToArguments", minimum = 0, maximum = 2)
    yield copy(
      childToId = childToId,
      childToArguments = childToArguments)

  def withoutSourcePos: ForkList = copy(
    sourcePos = None,
    workflow = workflow.withoutSourcePos)

  def withPositions(position: Position): ForkList =
    copy(workflow =
      workflow.withPositions(position / BranchId.ForkList))

  def adopt(outer: Workflow): ForkList = copy(
    workflow = workflow.copy(outer = Some(outer)))

  def reduceForAgent(agentPath: AgentPath, outer: Workflow): Instruction =
    if this.agentPath.contains(agentPath) || isVisibleForAgent(agentPath, outer) then
      copy(
        workflow = reuseIfEqual(workflow, workflow.reduceForAgent(agentPath)))
    else
      Gap(sourcePos = sourcePos)  // The agent will never touch this fork or its branches

  def isStartableOnAgent(agentPath: AgentPath): Boolean =
    // Any Agent or the controller can fork. The current Agent is okay.
    workflow.isStartableOnAgent(agentPath)

  def withoutBlocks: ForkList =
    copy(workflow = Workflow.empty)

  def workflow(branchId: BranchId): Checked[Workflow] =
    branchId match
      case BranchId.ForkList => Right(workflow)
      case _ => unknownBlock(branchId)

  def branchWorkflows: Seq[(BranchId, Workflow)] =
    Seq(BranchId.ForkList -> workflow)

  override def toString = s"ForkList()$sourcePosToString"


object ForkList:
  def checked(
    children: Expression,
    childToId: ExprFunction,
    childToArguments: ExprFunction,
    workflow: Workflow,
    agentPath: Option[AgentPath] = None,
    joinIfFailed: Boolean = false,
    sourcePos: Option[SourcePos] = None)
  : Checked[ForkList] =
    new ForkList(children, childToId, childToArguments, workflow, agentPath,
      joinIfFailed = joinIfFailed,sourcePos
    ).checked

  implicit val jsonEncoder: Encoder.AsObject[ForkList] = deriveEncoder[ForkList]
  implicit val jsonDecoder: Decoder[ForkList] = ConfiguredDecoder
    .derive[ForkList](useDefaults = true)
    .emap(_.checked.left.map(_.toString))
