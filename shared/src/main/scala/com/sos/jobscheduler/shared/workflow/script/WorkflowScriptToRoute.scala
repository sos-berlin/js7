package com.sos.jobscheduler.shared.workflow.script

import com.sos.jobscheduler.data.workflow.WorkflowScript.{End, ForkJoin, Goto, Job, NodeStatement, OnError}
import com.sos.jobscheduler.data.workflow.transition.{ForwardTransition, Transition}
import com.sos.jobscheduler.data.workflow.transitions.{ForkTransition, JoinTransition, SuccessErrorTransition}
import com.sos.jobscheduler.data.workflow.{NodeId, Workflow, WorkflowRoute, WorkflowScript}
import scala.collection.immutable.Seq
import scala.collection.mutable

/**
  * @author Joacim Zschimmer
  */
object WorkflowScriptToRoute {

  def toWorkflowRoute(route: WorkflowScript) =
    WorkflowRoute(route.startNode.id, route.nodes, transitionsOf(route))

  private def transitionsOf(route: WorkflowScript): Seq[Transition] = {
    val transitions = mutable.Buffer[Transition]()
    var current: Current = Current.Node(route.head.node)
    for (stmt ← route.statements.tail) {
      (current, stmt) match {
        case (Current.Node(node), stmt: Job) ⇒
          transitions += Transition(node.id, stmt.node.id, ForwardTransition)
          current = Current.Node(stmt.node)

        case (Current.Node(node), stmt: End) ⇒
          transitions += Transition(node.id, stmt.node.id, ForwardTransition)
          current = Current.End

        case (Current.End, stmt: NodeStatement) ⇒
          current = Current.Node(stmt.node)

        case (Current.Node(node), OnError(to)) ⇒
          current = Current.Transitions(next ⇒
            Transition(
              from = node.id :: Nil,
              to = next :: to :: Nil,
              SuccessErrorTransition
            ) :: Nil)

        case (Current.Node(node), Goto(to)) ⇒
          transitions += Transition(node.id, to, ForwardTransition)
          current = Current.End

        case (Current.Transitions(toTransition), Goto(to)) ⇒
          transitions ++= toTransition(to)
          current = Current.End

        case (Current.Node(node), ForkJoin(idToRoute)) ⇒
          current = Current.Transitions { next ⇒
            val (f, j) = Transition.forkJoin(
              forkNodeId = node.id,
              joinNodeId = next,
              idToRoute = idToRoute.map { case (k, v) ⇒ k → toWorkflowRoute(v) },
              ForkTransition,
              JoinTransition)
            f :: j :: Nil
          }

        case (Current.Transitions(toTransitions), stmt: Job) ⇒
          transitions ++= toTransitions(stmt.node.id)
          current = Current.Node(stmt.node)

        case (Current.Transitions(toTransitions), stmt: End) ⇒
          transitions ++= toTransitions(stmt.node.id)
          current = Current.End

        case _ ⇒
          throw new IllegalArgumentException(s"Not an allowed statement here: $stmt")
      }
    }
    current match {
      case _: Current.Node | Current.End ⇒
      case _ ⇒ throw new IllegalArgumentException(s"WorkflowScript must end with Job or End node")
    }
    transitions.toVector
  }

  private sealed trait Current
  private object Current {
    final case class Node(node: Workflow.Node) extends Current
    final case class Transitions(next: NodeId ⇒ Seq[Transition]) extends Current
    final case object End extends Current
  }
}
