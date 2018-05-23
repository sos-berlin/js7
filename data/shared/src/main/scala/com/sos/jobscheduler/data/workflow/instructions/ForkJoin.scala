package com.sos.jobscheduler.data.workflow.instructions

import cats.data.Validated
import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.base.circeutils.CirceObjectCodec
import com.sos.jobscheduler.base.circeutils.CirceUtils.deriveCodec
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.workflow.{Instruction, Position, Workflow}
import io.circe.generic.JsonCodec
import scala.collection.immutable.IndexedSeq
import scala.language.implicitConversions

/**
  * @author Joacim Zschimmer
  */
final case class ForkJoin(branches: IndexedSeq[ForkJoin.Branch])
extends Instruction
{
  for (idAndScript ← branches) ForkJoin.validateBranch(idAndScript).valueOr(throw _)

  def isPartiallyExecutableOnAgent(agentPath: AgentPath): Boolean =
    branches exists (_.workflow isPartiallyExecutableOnAgent agentPath)

  def isStartableOnAgent(agentPath: AgentPath): Boolean =
    // Any Agent or the master can fork. The current Agent is okay.
    branches exists (_.workflow isStartableOnAgent agentPath)

  //def isJoinableOnAgent(agentPath: AgentPath): Boolean =
  //  // If branches end on multiple Agents, only the Master can join the Orders
  //  branches.values forall (_ isEndingOnAgent agentPath)

  def workflowOption(branchId: Position.BranchId.Named): Option[Workflow] =
    branches collectFirst { case fj: ForkJoin.Branch if fj.id == branchId ⇒ fj.workflow }

  override def toString = s"ForkJoin(${branches.map(_.id).mkString(",")})"
}

object ForkJoin {
  implicit lazy val jsonCodec: CirceObjectCodec[ForkJoin] = deriveCodec[ForkJoin]

  def of(idAndWorkflows: (String, Workflow)*) =
    new ForkJoin(idAndWorkflows.map { case (id, workflow) ⇒ Branch(id, workflow) } .toVector)

  private def validateBranch(branch: Branch): Validated[RuntimeException, Branch] =
    if (branch.workflow.instructions exists (o ⇒ o.isInstanceOf[Goto]  || o.isInstanceOf[IfNonZeroReturnCodeGoto]))
      Invalid(new IllegalArgumentException(s"Fork/Join branch '${branch.id}' cannot contain a jump instruction like 'goto'"))
    else
      Valid(branch)

  @JsonCodec
  final case class Branch(id: Position.BranchId.Named, workflow: Workflow)
  object Branch {
    implicit def fromPair(pair: (Position.BranchId.Named, Workflow)) = new Branch(pair._1, pair._2)
  }
}
