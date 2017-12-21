package com.sos.jobscheduler.shared.workflow.script

import com.sos.jobscheduler.data.workflow.WorkflowScript.{End, ForkJoin, Goto, Job, OnError}
import com.sos.jobscheduler.data.workflow.transition.{ForwardTransition, Transition}
import com.sos.jobscheduler.data.workflow.transitions.{ForkTransition, JoinTransition, SuccessErrorTransition}
import com.sos.jobscheduler.data.workflow.{NodeId, WorkflowGraph, WorkflowScript}
import scala.collection.immutable.Seq
import scala.collection.mutable

/**
  * @author Joacim Zschimmer
  */
object WorkflowScriptToGraph {

  def workflowScriptToGraph(script: WorkflowScript) =
    WorkflowGraph(script.startNode.id, script.nodes, transitionsOf(script), originalScript = Some(script))

  private def transitionsOf(script: WorkflowScript): Seq[Transition] = {
    val transitions = mutable.Buffer[Transition]()
    var current: Current = Current.Node(script.head.node)
    for (stmt ← script.statements.tail) {
      (stmt, current) match {
        case (stmt: Job, Current.Node(node)) ⇒
          transitions += Transition(node.id, stmt.node.id, ForwardTransition)
          current = Current.Node(stmt.node)

        case (stmt: Job, Current.Transitions(toTransitions)) ⇒
          transitions ++= toTransitions(stmt.node.id)
          current = Current.Node(stmt.node)

        case (stmt: Job, Current.End) ⇒
          current = Current.Node(stmt.node)

        case (stmt: End, Current.Node(node)) ⇒
          transitions += Transition(node.id, stmt.node.id, ForwardTransition)
          current = Current.End

        case (stmt: End, Current.Transitions(toTransitions)) ⇒
          transitions ++= toTransitions(stmt.node.id)
          current = Current.End

        case (_: End, Current.End) ⇒

        case (ForkJoin(idToGraph), Current.Node(node)) ⇒
          current = Current.Transitions { next ⇒
            val (f, j) = Transition.forkJoin(
              forkNodeId = node.id,
              joinNodeId = next,
              idToGraph = idToGraph.map { case (k, v) ⇒ k → workflowScriptToGraph(v) },
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
    final case class Node(node: WorkflowGraph.Node) extends Current
    final case class Transitions(next: NodeId ⇒ Seq[Transition]) extends Current
    final case object End extends Current
  }
}
