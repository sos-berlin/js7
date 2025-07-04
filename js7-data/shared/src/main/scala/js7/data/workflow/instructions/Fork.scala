package js7.data.workflow.instructions

import io.circe.*
import io.circe.derivation.ConfiguredCodec
import io.circe.syntax.*
import js7.base.circeutils.CirceUtils.*
import js7.base.generic.GenericString
import js7.base.problem.Checked.Ops
import js7.base.problem.{Checked, Problem}
import js7.base.utils.Collections.implicits.*
import js7.base.utils.ScalaUtils.reuseIfEqual
import js7.base.utils.ScalaUtils.syntax.{RichBoolean, RichEitherIterable}
import js7.data.agent.AgentPath
import js7.data.source.SourcePos
import js7.data.value.expression.Expression
import js7.data.workflow.position.{BranchId, Position}
import js7.data.workflow.{Instruction, Workflow}
import org.jetbrains.annotations.TestOnly
import scala.collection.immutable.Map.Map1
import scala.language.implicitConversions

/**
  * @author Joacim Zschimmer
  */
final case class Fork(
  branches: Vector[Fork.Branch],
  agentPath: Option[AgentPath] = None,
  joinIfFailed: Boolean = false,
  sourcePos: Option[SourcePos] = None)
extends ForkInstruction:

  def withoutSourcePos: Fork = copy(
    sourcePos = None,
    branches = branches.map(b => b.copy(workflow = b.workflow.withoutSourcePos)))

  def withPositions(position: Position): Fork =
    copy(branches =
      branches.map(branch => branch.copy(
        workflow = branch.workflow.withPositions(position / branch.id.toBranchId))))

  def adopt(outer: Workflow): Fork = copy(
    branches = branches.map(o => o.copy(workflow = o.workflow.copy(outer = Some(outer)))))

  def reduceForAgent(agentPath: AgentPath, workflow: Workflow): Instruction =
    if this.agentPath.contains(agentPath) || isVisibleForAgent(agentPath, workflow) then
      copy(
        branches = for b <- branches yield
          reuseIfEqual(b, b.copy(
            workflow = b.workflow.reduceForAgent(agentPath))))
    else
      Gap(sourcePos = sourcePos)  // The agent will never touch this fork or its branches

  def isStartableOnAgent(agentPath: AgentPath): Boolean =
    // Any Agent or the controller can fork. The current Agent is okay.
    branches.exists(_.workflow.isStartableOnAgent(agentPath))

  //def isJoinableOnAgent(agentPath: AgentPath): Boolean =
  //  // If branches end on multiple Agents, only the Controller can join the Orders
  //  branches.values forall (_ isEndingOnAgent agentPath)

  //def startAgents: Set[AgentPath] =
  //  branches.flatMap(_.workflow.determinedExecutingAgent).toSet

  def withoutBlocks: Fork =
    copy(
      branches = branches.map(_.copy(
        workflow = Workflow.empty)))

  def workflow(branchId: BranchId): Checked[Workflow] =
    branchId match
      case BranchId.Named(name) if name.startsWith(BranchId.ForkPrefix) =>
        val id = ForkBranchId(name.drop(BranchId.ForkPrefix.length))
        branches.collectFirst { case Fork.Branch(`id`, workflow) => workflow }
          .fold(unknownBlock(branchId))(Right.apply)
      case _ =>
        unknownBlock(branchId)

  def branchWorkflows: Seq[(BranchId, Workflow)] =
    branches.map(b => b.id.toBranchId -> b.workflow)

  override def toString =
    s"Fork(${branches.map(_.id).mkString(",")})$sourcePosToString"


object Fork:
  private def apply(branches: Seq[Fork.Branch], sourcePos: Option[SourcePos]): Nothing =
    throw new NotImplementedError

  def forTest(branches: Seq[Fork.Branch], sourcePos: Option[SourcePos] = None): Fork =
    checked(branches, sourcePos = sourcePos).orThrow

  def checked(
    branches: Seq[Fork.Branch],
    agentPath: Option[AgentPath] = None,
    joinIfFailed: Boolean = false,
    sourcePos: Option[SourcePos] = None)
  : Checked[Fork] =
    val checkedResult = branches.view
      .flatMap(b => b.result.keys.view.map(_ -> b.id))
      .checkUniqueness_(_._1)(_
        .view
        .mapValues(_.map(_._2))
        .map: (name, branchIds) =>
          Problem(s"Result name '$name' is used duplicately in Fork branches " +
            s"${branchIds.mkString("'", "' , '", "'")}")
        .reduce(Problem.combine))
    val checkedUniqueBranchIds = branches.duplicateKeys(_.id)
      .map(dups => DuplicatedBranchIdsInForkProblem(dups.keys.toSeq))
      .toLeft(())
    Seq(checkedResult, checkedUniqueBranchIds)
      .reduceLeftEither
      .map(_ => new Fork(branches.toVector, agentPath, joinIfFailed = joinIfFailed, sourcePos))

  def of(idAndWorkflows: (String, Workflow)*) =
    new Fork(
      idAndWorkflows.map: (id, workflow) =>
        Branch(ForkBranchId(id), workflow)
      .toVector)


  final case class Branch(id: ForkBranchId, workflow: Workflow):
    def result: Map[String, Expression] =
      workflow.result.getOrElse(Map.empty)

  object Branch:
    implicit def fromPair(pair: (String, Workflow)): Branch =
      new Branch(ForkBranchId(pair._1), pair._2)

    given jsonCodec: Codec.AsObject[Branch] =
      ConfiguredCodec.derive[Branch](useDefaults = true)

  //implicit lazy val jsonCodec: CirceObjectCodec[Fork] = deriveCodec[Fork]
  given Encoder.AsObject[Fork] =
    o => JsonObject(
      "branches" -> o.branches.asJson,
      "sourcePos" -> o.sourcePos.asJson,
      "agentPath" -> o.agentPath.asJson,
      "joinIfFailed" -> o.joinIfFailed.?.asJson)

  given Decoder[Fork] =
    c => for
      branches <- c.get[Vector[Fork.Branch]]("branches")
      sourcePos <- c.get[Option[SourcePos]]("sourcePos")
      agentPath <- c.get[Option[AgentPath]]("agentPath")
      joinIfFailed <- c.getOrElse[Boolean]("joinIfFailed")(false)
      fork <- checked(branches, agentPath, joinIfFailed, sourcePos)
        .toDecoderResult(c.history)
    yield
      fork

  final case class DuplicatedBranchIdsInForkProblem(branchIds: Seq[ForkBranchId])
  extends Problem.Coded:
    def arguments: Map[String, String] = Map1(
      "branchIds", branchIds.mkString(", "))


/** ForkBranchId("x").string == BranchId("fork+x") */
final case class ForkBranchId(string: String) extends GenericString:
  def toBranchId: BranchId.Named = BranchId.fork(string)

object ForkBranchId extends GenericString.Checked_[ForkBranchId]:
  def unchecked(string: String) = new ForkBranchId(string)

  @TestOnly
  implicit def fromString(string: String): ForkBranchId =
    apply(string)
