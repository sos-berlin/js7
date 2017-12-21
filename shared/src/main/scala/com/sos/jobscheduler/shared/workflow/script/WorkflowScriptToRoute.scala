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

  def workflowScriptToRoute(route: WorkflowScript) =
    WorkflowRoute(route.startNode.id, route.nodes, transitionsOf(route))

  private def transitionsOf(route: WorkflowScript): Seq[Transition] = {
    val transitions = mutable.Buffer[Transition]()
    var current: Current = Current.Node(route.head.node)
    for (stmt ← route.statements.tail) {
      (stmt, current) match {
        case (stmt: Job, Current.Node(node)) ⇒
          transitions += Transition(node.id, stmt.node.id, ForwardTransition)
          current = Current.Node(stmt.node)

        case (stmt: Job, Current.Transitions(toTransitions)) ⇒
          transitions ++= toTransitions(stmt.node.id)
          current = Current.Node(stmt.node)

        case (stmt: End, Current.Node(node)) ⇒
          transitions += Transition(node.id, stmt.node.id, ForwardTransition)
          current = Current.End

        case (stmt: End, Current.Transitions(toTransitions)) ⇒
          transitions ++= toTransitions(stmt.node.id)
          current = Current.End

        case (stmt: NodeStatement, Current.End) ⇒
          current = Current.Node(stmt.node)

        case (ForkJoin(idToRoute), Current.Node(node)) ⇒
          current = Current.Transitions { next ⇒
            val (f, j) = Transition.forkJoin(
              forkNodeId = node.id,
              joinNodeId = next,
              idToRoute = idToRoute.map { case (k, v) ⇒ k → workflowScriptToRoute(v) },
              ForkTransition,
              JoinTransition)
            f :: j :: Nil
          }

        case (OnError(to), Current.Node(node)) ⇒
          current = Current.Transitions(next ⇒
            if (next == to)
              Transition(node.id, next, ForwardTransition) :: Nil
            else
              Transition(
                from = node.id :: Nil,
                to = next :: to :: Nil,
                SuccessErrorTransition
              ) :: Nil)

        case (Goto(to), Current.Node(node)) ⇒
          transitions += Transition(node.id, to, ForwardTransition)
          current = Current.End

        case (Goto(to), Current.Transitions(toTransition)) ⇒
          transitions ++= toTransition(to)
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
