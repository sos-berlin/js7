package js7.data.workflow.instructions

import io.circe._
import io.circe.syntax._
import js7.base.circeutils.CirceUtils._
import js7.base.generic.GenericString
import js7.base.problem.Checked.Ops
import js7.base.problem.{Checked, Problem}
import js7.base.utils.Collections.implicits._
import js7.base.utils.ScalaUtils.reuseIfEqual
import js7.base.utils.StackTraces.StackTraceThrowable
import js7.data.agent.AgentPath
import js7.data.source.SourcePos
import js7.data.workflow.instructions.Fork._
import js7.data.workflow.position.{BranchId, Position}
import js7.data.workflow.{Instruction, Workflow}
import scala.language.implicitConversions

/**
  * @author Joacim Zschimmer
  */
final case class Fork private(
  branches: IndexedSeq[Fork.Branch],
  agentPath: Option[AgentPath] = None,
  sourcePos: Option[SourcePos] = None)
extends ForkInstruction
{
  // TODO Fork.checked(..): Checked[Fork]
  for (dups <- branches.duplicateKeys(_.id))
    throw DuplicatedBranchIdsInForkProblem(dups.keys.toSeq).throwable.appendCurrentStackTrace
  for (idAndScript <- branches) Fork.validateBranch(idAndScript).orThrow

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
  private def apply(branches: IndexedSeq[Fork.Branch], sourcePos: Option[SourcePos]) =
    throw new NotImplementedError

  def forTest(branches: IndexedSeq[Fork.Branch], sourcePos: Option[SourcePos] = None): Fork =
    checked(branches, sourcePos = sourcePos).orThrow

  def checked(
    branches: IndexedSeq[Fork.Branch],
    agentPath: Option[AgentPath] = None,
    sourcePos: Option[SourcePos] = None)
  : Checked[Fork] =
    Right(new Fork(branches, agentPath, sourcePos))

  def of(idAndWorkflows: (String, Workflow)*) =
    new Fork(
      idAndWorkflows
        .map { case (id, workflow) => Branch(Branch.Id(id), workflow) }
        .toVector)

  private def validateBranch(branch: Branch): Checked[Branch] =
    if (branch.workflow.instructions.exists(o => o.isInstanceOf[Goto] || o.isInstanceOf[IfFailedGoto]))
      Left(Problem(s"Fork/Join branch '${branch.id}' cannot contain a jump instruction like 'goto'"))
    else
      Right(branch)

  final case class Branch(id: Branch.Id, workflow: Workflow)
  object Branch {
    implicit def fromPair(pair: (Id, Workflow)): Branch =
      new Branch(pair._1, pair._2)

    /** Branch.Id("x").string == BranchId("fork+x") */
    final case class Id(string: String) extends GenericString {
      def toBranchId = BranchId.fork(string)
    }
    object Id extends GenericString.Checked_[Id] {
      def unchecked(string: String) = new Id(string)
      implicit def fromString(string: String) = apply(string)
    }

    implicit val jsonCodec = deriveCodec[Branch]
  }

  //implicit lazy val jsonCodec: CirceObjectCodec[Fork] = deriveCodec[Fork]
  implicit val jsonEncoder: Encoder.AsObject[Fork] =
    o => JsonObject(
      "branches" -> o.branches.asJson,
      "sourcePos" -> o.sourcePos.asJson,
      "agentPath" -> o.agentPath.asJson)

  implicit val jsonDecoder: Decoder[Fork] =
    c => for {
      branches <- c.get[IndexedSeq[Fork.Branch]]("branches")
      sourcePos <- c.get[Option[SourcePos]]("sourcePos")
      agentPath <- c.get[Option[AgentPath]]("agentPath")
      fork <- checked(branches, agentPath, sourcePos).toDecoderResult(c.history)
    } yield fork

  private case class DuplicatedBranchIdsInForkProblem(branchIds: Seq[Fork.Branch.Id]) extends Problem.Coded {
    def arguments = Map(
      "branchIds" -> branchIds.mkString(", ")
    )
  }
}
