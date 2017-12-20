package com.sos.jobscheduler.shared.workflow.script

import com.sos.jobscheduler.data.workflow.Workflow.JobNode
import com.sos.jobscheduler.data.workflow.WorkflowScript.{End, ForkJoin, Goto, Job, OnError}
import com.sos.jobscheduler.data.workflow.test.ForkTestSetting._
import com.sos.jobscheduler.data.workflow.transition.{ForwardTransition, Transition}
import com.sos.jobscheduler.data.workflow.transitions.SuccessErrorTransition
import com.sos.jobscheduler.data.workflow.{NodeId, WorkflowRoute, WorkflowScript}
import com.sos.jobscheduler.shared.workflow.script.WorkflowScriptToRoute.toWorkflowRoute
import org.scalatest.FreeSpec
import scala.collection.immutable._
import scala.language.implicitConversions

/**
  * @author Joacim Zschimmer
  */
final class WorkflowScriptToRouteTest extends FreeSpec {

  "Single job" in {
    val script = WorkflowScript(Job(A.id, AAgentJobPath) :: Nil)
    assert(toWorkflowRoute(script) ==
      WorkflowRoute(start = A.id, nodes = A :: Nil, transitions = Nil))
  }

  "WorkflowScript of multiple jobs" in {
    val script = WorkflowScript(List(
      Job(A.id, AAgentJobPath),
      Job(D.id, AAgentJobPath),
      Job(G.id, AAgentJobPath)))
    assert(toWorkflowRoute(script) ==
      WorkflowRoute(start = A.id,
        nodes = List(A, D, G),
        transitions = List(
          Transition(A.id, D.id, ForwardTransition),
          Transition(D.id, G.id, ForwardTransition))))
  }

  "Spaghetti with OnError and Goto" in {
    val X = JobNode(NodeId("X"), AAgentJobPath)
    val script = WorkflowScript(List(
      Job(A.id, AAgentJobPath),
      OnError(G.id),
      Job(D.id, AAgentJobPath),
      OnError(END.id),
      Goto(G.id),
      Job(X.id, AAgentJobPath),
      End(END.id),
      Job(G.id, AAgentJobPath),
      Goto(X.id)))
    val expected = WorkflowRoute(start = A.id,
      nodes = List(A, D, X, END, G),
      transitions = List(
        Transition(from = List(A.id), to = List(D.id, G.id), SuccessErrorTransition),
        Transition(from = List(D.id), to = List(G.id, END.id), SuccessErrorTransition),
        Transition(X.id, END.id, ForwardTransition),
        Transition(G.id, X.id, ForwardTransition)))
    assert(toWorkflowRoute(script) == expected)
    assert(expected.linearPath == Some(List(A.id, D.id, G.id, X.id, END.id)))
    assert(expected.end == Some(END.id))
  }

  "Workflow from TestForkSetting" in {
    val script = WorkflowScript(List(
      Job(A.id, AAgentJobPath),
      ForkJoin(ListMap(
        WorkflowRoute.Id("🥕") → WorkflowScript(List(
          Job(Bx.id, AAgentJobPath),
          Job(Cx.id, AAgentJobPath))),
        WorkflowRoute.Id("🍋") → WorkflowScript(List(
          Job(By.id, AAgentJobPath),
          Job(Cy.id, BAgentJobPath))))),
      Job(D.id, AAgentJobPath),
      ForkJoin(ListMap(
        WorkflowRoute.Id("🥕") → WorkflowScript(List(
          Job(Ex.id, AAgentJobPath),
          Job(Fx.id, AAgentJobPath))),
        WorkflowRoute.Id("🍋") → WorkflowScript(List(
          Job(Ey.id, AAgentJobPath),
          Job(Fy.id, AAgentJobPath))))),
      Job(G.id, AAgentJobPath),
      End(END.id)))
    assert(toWorkflowRoute(script) == TestWorkflow.route)
  }
}
