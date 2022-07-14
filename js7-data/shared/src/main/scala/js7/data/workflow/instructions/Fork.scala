package js7.data.workflow.instructions

import io.circe.*
import io.circe.generic.extras.Configuration.default.withDefaults
import io.circe.generic.extras.semiauto.deriveConfiguredCodec
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
import js7.data.workflow.instructions.Fork.*
import js7.data.workflow.position.{BranchId, Position}
import js7.data.workflow.{Instruction, Workflow}
import scala.language.implicitConversions

/**
  * @author Joacim Zschimmer
  */
final case class Fork(
  branches: Vector[Fork.Branch],
  agentPath: Option[AgentPath] = None,
  joinIfFailed: Boolean = false,
  sourcePos: Option[SourcePos] = None)
extends ForkInstruction
{
  def withoutSourcePos = copy(
    sourcePos = None,
    branches = branches.map(b => b.copy(workflow = b.workflow.withoutSourcePos)))

  override def withPositions(position: Position): Instruction =
    copy(branches =
      branches.map(branch => branch.copy(
        workflow = branch.workflow withPositions position / branch.id.toBranchId)))

  override def adopt(outer: Workflow) = copy(
    branches = branches.map(o => o.copy(workflow = o.workflow.copy(outer = Some(outer)))))

  override def reduceForAgent(agentPath: AgentPath, workflow: Workflow) =
    if (isVisibleForAgent(agentPath, workflow))
      copy(
        branches = for (b <- branches) yield
          reuseIfEqual(b, b.copy(
            workflow = b.workflow.reduceForAgent(agentPath))))
    else
      Gap(sourcePos = sourcePos)  // The agent will never touch this fork or its branches

  def isStartableOnAgent(agentPath: AgentPath): Boolean =
    // Any Agent or the controller can fork. The current Agent is okay.
    branches.exists(_.workflow isStartableOnAgent agentPath)

  //def isJoinableOnAgent(agentPath: AgentPath): Boolean =
  //  // If branches end on multiple Agents, only the Controller can join the Orders
  //  branches.values forall (_ isEndingOnAgent agentPath)

  //def startAgents: Set[AgentPath] =
  //  branches.flatMap(_.workflow.determinedExecutingAgent).toSet

  override def workflow(branchId: BranchId) =
    branchId match {
      case BranchId.Named(name) if name startsWith BranchId.ForkPrefix =>
        val id = Branch.Id(name drop BranchId.ForkPrefix.length)
        branches.collectFirst { case Fork.Branch(`id`, workflow) => workflow }
          .fold(super.workflow(branchId))(Right.apply)
      case _ =>
        super.workflow(branchId)
    }

  override def branchWorkflows = branches.map(b => b.id.toBranchId -> b.workflow)

  override def toString = s"Fork(${branches.map(_.id).mkString(",")})$sourcePosToString"
}

object Fork
{
  private def apply(branches: Seq[Fork.Branch], sourcePos: Option[SourcePos]) =
    throw new NotImplementedError

  def forTest(branches: Seq[Fork.Branch], sourcePos: Option[SourcePos] = None): Fork =
    checked(branches, sourcePos = sourcePos).orThrow

  def checked(
    branches: Seq[Fork.Branch],
    agentPath: Option[AgentPath] = None,
    joinIfFailed: Boolean = false,
    sourcePos: Option[SourcePos] = None)
  : Checked[Fork] = {
    val checkedResult = branches.view
      .flatMap(b => b.result.keys.view.map(_ -> b.id))
      .checkUniqueness_(_._1)(_
        .view
        .mapValues(_.map(_._2))
        .map { case (name, branchIds) =>
          Problem(s"Result name '$name' is used duplicately in Fork branches " +
            s"${branchIds.mkString("'", "' , '", "'")}")
        }
        .reduce(Problem.combine))
    val checkedUniqueBranchIds = branches.duplicateKeys(_.id)
      .map(dups => DuplicatedBranchIdsInForkProblem(dups.keys.toSeq))
      .toLeft(())
    Seq(checkedResult, checkedUniqueBranchIds)
      .reduceLeftEither
      .map(_ => new Fork(branches.toVector, agentPath, joinIfFailed = joinIfFailed, sourcePos))
  }

  def of(idAndWorkflows: (String, Workflow)*) =
    new Fork(
      idAndWorkflows
        .map { case (id, workflow) => Branch(Branch.Id(id), workflow) }
        .toVector)

  final case class Branch(id: Branch.Id, workflow: Workflow)
  {
    def result: Map[String, Expression] =
      workflow.result.getOrElse(Map.empty)
  }
  object Branch {
    implicit def fromPair(pair: (String, Workflow)): Branch =
      new Branch(Branch.Id(pair._1), pair._2)

    /** Branch.Id("x").string == BranchId("fork+x") */
    final case class Id(string: String) extends GenericString {
      def toBranchId = BranchId.fork(string)
    }
    object Id extends GenericString.Checked_[Id] {
      def unchecked(string: String) = new Id(string)
      implicit def fromString(string: String) = apply(string)
    }

    private implicit val customConfig = withDefaults
    implicit val jsonCodec = deriveConfiguredCodec[Branch]
  }

  //implicit lazy val jsonCodec: CirceObjectCodec[Fork] = deriveCodec[Fork]
  implicit val jsonEncoder: Encoder.AsObject[Fork] =
    o => JsonObject(
      "branches" -> o.branches.asJson,
      "sourcePos" -> o.sourcePos.asJson,
      "agentPath" -> o.agentPath.asJson,
      "joinIfFailed" -> o.joinIfFailed.?.asJson)

  implicit val jsonDecoder: Decoder[Fork] =
    c => for {
      branches <- c.get[Vector[Fork.Branch]]("branches")
      sourcePos <- c.get[Option[SourcePos]]("sourcePos")
      agentPath <- c.get[Option[AgentPath]]("agentPath")
      joinIfFailed <- c.getOrElse[Boolean]("joinIfFailed")(false)
      fork <- checked(branches, agentPath, joinIfFailed, sourcePos)
        .toDecoderResult(c.history)
    } yield fork

  final case class DuplicatedBranchIdsInForkProblem(branchIds: Seq[Fork.Branch.Id]) extends Problem.Coded {
    def arguments = Map(
      "branchIds" -> branchIds.mkString(", ")
    )
  }
}
