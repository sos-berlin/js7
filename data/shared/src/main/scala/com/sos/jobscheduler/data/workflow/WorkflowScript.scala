package com.sos.jobscheduler.data.workflow

import com.sos.jobscheduler.base.circeutils.CirceCodec
import com.sos.jobscheduler.base.circeutils.CirceUtils.deriveCirceCodec
import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.data.workflow.WorkflowScript._
import scala.collection.immutable.{ListMap, Seq}

/**
  * @author Joacim Zschimmer
  */
final case class WorkflowScript(statements: Seq[Statement]) {
  val head: Job = statements.headOption match {
    case None ⇒ throw new IllegalArgumentException("WorkflowScript must not be empty")
    case Some(job: Job) ⇒ job
    case Some(_) ⇒ throw new IllegalArgumentException("First statement of a route must be a job")
  }

  def startNode = head.node
  def nodes = statements.flatMap(_.nodes)
}

object WorkflowScript {
  implicit val jsonCodec: CirceCodec[WorkflowScript] = {
    implicit val statementJsonCodec = TypedJsonCodec[Statement](
      Subtype(deriveCirceCodec[Job]),
      Subtype(deriveCirceCodec[End]),
      Subtype(deriveCirceCodec[ForkJoin]),
      Subtype(deriveCirceCodec[OnError]))
    deriveCirceCodec[WorkflowScript]
  }

  sealed trait Statement {
    def nodes: Seq[Workflow.Node]
  }

  sealed trait NodeStatement extends Statement {
    def node: Workflow.Node

    final def nodes = node :: Nil
  }

  final case class Job(nodeId: NodeId, job: AgentJobPath) extends NodeStatement {
    val node = Workflow.JobNode(nodeId, job)
  }

  final case class End(nodeId: NodeId) extends NodeStatement {
    val node = Workflow.EndNode(nodeId)
  }

  final case class ForkJoin(idToRoute: ListMap[WorkflowRoute.Id, WorkflowScript])
  extends Statement {
    def nodes = idToRoute.values.flatMap(_.nodes).toVector
  }

  final case class OnError(to: NodeId) extends Statement {
    def nodes = Nil
  }

  final case class Goto(to: NodeId) extends Statement {
    def nodes = Nil
  }
}

