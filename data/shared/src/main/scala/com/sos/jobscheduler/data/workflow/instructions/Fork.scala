package com.sos.jobscheduler.data.workflow.instructions

import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.base.generic.GenericString
import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.base.utils.Collections.implicits._
import com.sos.jobscheduler.data.agent.AgentRefPath
import com.sos.jobscheduler.data.source.SourcePos
import com.sos.jobscheduler.data.workflow.instructions.Fork._
import com.sos.jobscheduler.data.workflow.position.BranchId
import com.sos.jobscheduler.data.workflow.{Instruction, Workflow}
import io.circe._
import io.circe.syntax._
import scala.collection.immutable.{IndexedSeq, Seq}
import scala.language.implicitConversions

/**
  * @author Joacim Zschimmer
  */
final case class Fork private(branches: IndexedSeq[Fork.Branch], sourcePos: Option[SourcePos] = None)
extends Instruction
{
  // TODO Fork.checked(..): Checked[Fork]
  for (dups <- branches.duplicateKeys(_.id))
    throw DuplicatedBranchIdsInForkProblem(dups.keys.toImmutableSeq).throwable
  for (idAndScript <- branches) Fork.validateBranch(idAndScript).orThrow

  def withoutSourcePos = copy(
    sourcePos = None,
    branches = branches.map(b => b.copy(workflow = b.workflow.withoutSourcePos)))

  override def adopt(outer: Workflow) = copy(
    branches = branches.map(o => o.copy(workflow = o.workflow.copy(outer = Some(outer)))))

  def isPartiallyExecutableOnAgent(agentRefPath: AgentRefPath): Boolean =
    branches exists (_.workflow isPartiallyExecutableOnAgent agentRefPath)

  def isStartableOnAgent(agentRefPath: AgentRefPath): Boolean =
    // Any Agent or the master can fork. The current Agent is okay.
    branches exists (_.workflow isStartableOnAgent agentRefPath)

  //def isJoinableOnAgent(agentRefPath: AgentRefPath): Boolean =
  //  // If branches end on multiple Agents, only the Master can join the Orders
  //  branches.values forall (_ isEndingOnAgent agentRefPath)

  //def startAgents: Set[AgentRefPath] =
  //  branches.flatMap(_.workflow.determinedExecutingAgent).toSet

  override def workflow(branchId: BranchId) = {
    branchId match {
      case BranchId.Named(name) if name startsWith BranchId.ForkPrefix =>
        val id = Branch.Id(name drop BranchId.ForkPrefix.length)
        branches.collectFirst { case Fork.Branch(`id`, workflow) => workflow }
          .fold(super.workflow(branchId))(Valid.apply)
      case _ =>
        super.workflow(branchId)
    }
  }

  override def branchWorkflows = branches map (b => b.id.toBranchId -> b.workflow)

  override def toString = s"Fork(${branches.map(_.id).mkString(",")})"
}

object Fork
{
  private def apply(branches: IndexedSeq[Fork.Branch], sourcePos: Option[SourcePos] = None) =
    throw new NotImplementedError

  def forTest(branches: IndexedSeq[Fork.Branch], sourcePos: Option[SourcePos] = None): Fork =
    checked(branches, sourcePos).orThrow

  def checked(branches: IndexedSeq[Fork.Branch], sourcePos: Option[SourcePos] = None): Checked[Fork] =
    Valid(new Fork(branches, sourcePos))

  def of(idAndWorkflows: (String, Workflow)*) =
    new Fork(idAndWorkflows.map { case (id, workflow) => Branch(Branch.Id(id), workflow) } .toVector)

  private def validateBranch(branch: Branch): Checked[Branch] =
    if (branch.workflow.instructions exists (o => o.isInstanceOf[Goto] || o.isInstanceOf[IfNonZeroReturnCodeGoto]))
      Invalid(Problem(s"Fork/Join branch '${branch.id}' cannot contain a jump instruction like 'goto'"))
    else
      Valid(branch)

  final case class Branch(id: Branch.Id, workflow: Workflow)
  object Branch {
    implicit def fromPair(pair: (Id, Workflow)): Branch =
      new Branch(pair._1, pair._2)

    /** Branch.Id("x").string == BranchId("fork+x") */
    final case class Id(string: String) extends GenericString {
      def toBranchId = BranchId.fork(string)
    }
    object Id extends GenericString.Checked_[Id] {
      implicit def unchecked(string: String) = new Id(string)
    }

    implicit val jsonCodec = deriveCodec[Branch]
  }

  //implicit lazy val jsonCodec: CirceObjectCodec[Fork] = deriveCodec[Fork]
  implicit val jsonEncoder: ObjectEncoder[Fork] =
    o => JsonObject(
      "branches" -> o.branches.asJson,
      "sourcePos" -> o.sourcePos.asJson)

  implicit val jsonDecoder: Decoder[Fork] =
    c => for {
      branches <- c.get[IndexedSeq[Fork.Branch]]("branches")
      sourcePos <- c.get[Option[SourcePos]]("sourcePos")
      fork <- checked(branches, sourcePos).toDecoderResult
    } yield fork

  private case class DuplicatedBranchIdsInForkProblem(branchIds: Seq[Fork.Branch.Id]) extends Problem.Coded {
    def arguments = Map(
      "branchIds" -> branchIds.mkString(", ")
    )
  }
}
