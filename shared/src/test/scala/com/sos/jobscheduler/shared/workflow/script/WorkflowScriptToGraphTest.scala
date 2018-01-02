package com.sos.jobscheduler.shared.workflow.script

import com.sos.jobscheduler.data.workflow.WorkflowGraph.JobNode
import com.sos.jobscheduler.data.workflow.WorkflowScript.{End, Goto, IfError, Job}
import com.sos.jobscheduler.data.workflow.test.ForkTestSetting._
import com.sos.jobscheduler.data.workflow.transition.{ForwardTransition, Transition}
import com.sos.jobscheduler.data.workflow.transitions.{ForkTransition, SuccessErrorTransition}
import com.sos.jobscheduler.data.workflow.{NodeId, WorkflowGraph, WorkflowScript}
import com.sos.jobscheduler.shared.workflow.script.WorkflowScriptToGraph.workflowScriptToGraph
import org.scalatest.FreeSpec
import scala.collection.immutable._
import scala.language.implicitConversions

/**
  * @author Joacim Zschimmer
  */
final class WorkflowScriptToGraphTest extends FreeSpec {

  "Single job" in {
    val script = WorkflowScript(Job(A.id, AAgentJobPath) :: Nil)
    assert(workflowScriptToGraph(script) ==
      WorkflowGraph(start = A.id, nodes = A :: Nil, transitions = Nil, Some(script)))
  }

  "WorkflowScript of multiple jobs" in {
    val script = WorkflowScript(List(
      Job(A.id, AAgentJobPath),
      Job(D.id, AAgentJobPath),
      Job(G.id, AAgentJobPath)))
    assert(workflowScriptToGraph(script) ==
      WorkflowGraph(start = A.id,
        nodes = List(A, D, G),
        transitions = List(
          Transition(A.id, D.id, ForwardTransition),
          Transition(D.id, G.id, ForwardTransition)),
        Some(script)))
  }

  "Spaghetti with IfError and Goto" in {
    val X = JobNode(NodeId("X"), AAgentJobPath)
    val script = WorkflowScript(List(
      Job(A.id, AAgentJobPath),
      IfError(G.id),
      Job(D.id, AAgentJobPath),
      IfError(END.id),
      Goto(G.id),
      Job(X.id, AAgentJobPath),
      End(END.id),
      Job(G.id, AAgentJobPath),
      Goto(X.id)))
    val expected = WorkflowGraph(start = A.id,
      nodes = List(A, D, X, END, G),
      transitions = List(
        Transition(from = List(A.id), to = List(D.id, G.id), SuccessErrorTransition),
        Transition(from = List(D.id), to = List(G.id, END.id), SuccessErrorTransition),
        Transition(X.id, END.id, ForwardTransition),
        Transition(G.id, X.id, ForwardTransition)),
      Some(script))
    assert(workflowScriptToGraph(script) == expected)
    assert(expected.linearPath == Some(List(A.id, D.id, G.id, X.id, END.id)))
    assert(expected.end == Some(END.id))
  }

  "Workflow from TestForkSetting" in {
    val compiled = workflowScriptToGraph(TestWorkflowScript)
    for ((a, b) ← compiled.transitions.zip(TestWorkflow.graph.transitions).filter(_._1.transitionType == ForkTransition)) {
      val x = a == b
      assert(x)
    }
    assert(workflowScriptToGraph(TestWorkflowScript) == TestWorkflow.graph)
  }
}
