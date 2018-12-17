package com.sos.jobscheduler.data.workflow.instructions

import cats.data.Validated
import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.base.circeutils.CirceObjectCodec
import com.sos.jobscheduler.base.circeutils.CirceUtils.deriveCodec
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.base.utils.Collections.implicits.RichTraversable
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.workflow.position.{BranchId, Position}
import com.sos.jobscheduler.data.workflow.{Instruction, Workflow}
import io.circe.generic.JsonCodec
import scala.collection.immutable.IndexedSeq
import scala.language.implicitConversions

/**
  * @author Joacim Zschimmer
  */
final case class Fork(branches: IndexedSeq[Fork.Branch])
extends Instruction
{
  locally {
    val dups = branches.duplicateKeys(_.id)
    if (dups.nonEmpty) throw Problem(s"Non-unique branch IDs in fork: ${dups.mkString(", ")}").throwable  // To Fork.checked(..): Checked[Fork]
  }

  for (idAndScript ← branches) Fork.validateBranch(idAndScript).valueOr(throw _)

  override def adopt(outer: Workflow) = copy(
    branches = branches.map(o ⇒ o.copy(workflow = o.workflow.copy(outer = Some(outer)))))

  def isPartiallyExecutableOnAgent(agentPath: AgentPath): Boolean =
    branches exists (_.workflow isPartiallyExecutableOnAgent agentPath)

  def isStartableOnAgent(agentPath: AgentPath): Boolean =
    // Any Agent or the master can fork. The current Agent is okay.
    branches exists (_.workflow isStartableOnAgent agentPath)

  //def isJoinableOnAgent(agentPath: AgentPath): Boolean =
  //  // If branches end on multiple Agents, only the Master can join the Orders
  //  branches.values forall (_ isEndingOnAgent agentPath)

  //def startAgents: Set[AgentPath] =
  //  branches.flatMap(_.workflow.determinedExecutingAgent).toSet

  override def workflow(branchId: BranchId) =
    branches.collectFirst({ case fj: Fork.Branch if fj.id == branchId ⇒ fj.workflow })
      .fold(super.workflow(branchId))(Valid.apply)

  override def flattenedWorkflows(outer: Position) =
    branches.toList flatMap (b ⇒ b.workflow.flattenedWorkflowsOf(outer / b.id))

  override def flattenedInstructions(outer: Position) =
    branches flatMap (b ⇒ b.workflow.flattenedInstructions(outer / b.id))

  override def toString = s"Fork(${branches.map(_.id).mkString(",")})"
}

object Fork {
  implicit lazy val jsonCodec: CirceObjectCodec[Fork] = deriveCodec[Fork]

  def of(idAndWorkflows: (String, Workflow)*) =
    new Fork(idAndWorkflows.map { case (id, workflow) ⇒ Branch(id, workflow) } .toVector)

  private def validateBranch(branch: Branch): Validated[RuntimeException, Branch] =
    if (branch.workflow.instructions exists (o ⇒ o.isInstanceOf[Goto]  || o.isInstanceOf[IfNonZeroReturnCodeGoto]))
      Invalid(new IllegalArgumentException(s"Fork/Join branch '${branch.id}' cannot contain a jump instruction like 'goto'"))
    else
      Valid(branch)

  @JsonCodec
  final case class Branch(id: BranchId.Named, workflow: Workflow)
  object Branch {
    implicit def fromPair(pair: (BranchId.Named, Workflow)) = new Branch(pair._1, pair._2)
  }
}
