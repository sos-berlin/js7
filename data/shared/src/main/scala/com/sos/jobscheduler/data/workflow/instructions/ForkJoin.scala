package com.sos.jobscheduler.data.workflow.instructions

import cats.data.Validated
import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.base.circeutils.CirceCodec
import com.sos.jobscheduler.base.circeutils.CirceUtils.deriveCirceCodec
import com.sos.jobscheduler.base.utils.MapDiff
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.order.OrderEvent.{OrderDetachable, OrderForked, OrderJoined}
import com.sos.jobscheduler.data.order.{Order, Outcome}
import com.sos.jobscheduler.data.workflow.instructions.Instructions.ifProcessedThenOrderMoved
import com.sos.jobscheduler.data.workflow.{EventInstruction, OrderContext, Position, Workflow}
import io.circe.generic.JsonCodec
import scala.collection.immutable.IndexedSeq
import scala.language.implicitConversions

/**
  * @author Joacim Zschimmer
  */
final case class ForkJoin(branches: IndexedSeq[ForkJoin.Branch])
extends EventInstruction
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

  def toEvent(order: Order[Order.State], context: OrderContext) =
    order.ifState[Order.Ready].map(order ⇒
      order.id <-: OrderForked(
        for (branch ← branches) yield
          OrderForked.Child(branch.id, order.id / branch.id.childId, MapDiff.empty)))
    .orElse(
      order.ifState[Order.Join].flatMap(order ⇒
        //orderEntry.instruction match {
        //  case forkJoin: Instruction.ForkJoin if forkJoin isJoinableOnAgent ourAgentPath ⇒
        if (order.isAttachedToAgent)
          Some(order.id <-: OrderDetachable)  //
        else if (order.state.joinOrderIds map context.idToOrder forall context.childOrderEnded)
          Some(order.id <-: OrderJoined(MapDiff.empty, Outcome.Good(true)))
        else
          None))
    .orElse(
      ifProcessedThenOrderMoved(order, context))

  override def toShortString = s"ForkJoin(${branches.map(_.id).mkString(",")})"
}

object ForkJoin {
  implicit lazy val jsonCodec: CirceCodec[ForkJoin] = deriveCirceCodec[ForkJoin]

  def of(idAndWorkflows: (String, Workflow)*) =
    new ForkJoin(idAndWorkflows.map { case (id, workflow) ⇒ Branch(id, workflow) } .toVector)

  private def validateBranch(branch: Branch): Validated[RuntimeException, Branch] =
    if (branch.workflow.instructions exists (o ⇒ o.isInstanceOf[Goto]  || o.isInstanceOf[IfErrorGoto]))
      Invalid(new IllegalArgumentException(s"Fork/Join branch '${branch.id}' cannot contain a jump instruction like 'goto' or 'ifError'"))
    else
      Valid(branch)

  @JsonCodec
  final case class Branch(id: Position.BranchId.Named, workflow: Workflow)
  object Branch {
    implicit def fromPair(pair: (Position.BranchId.Named, Workflow)) = new Branch(pair._1, pair._2)
  }
}
