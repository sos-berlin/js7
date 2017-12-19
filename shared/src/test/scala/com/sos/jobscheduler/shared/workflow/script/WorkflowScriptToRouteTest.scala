package com.sos.jobscheduler.shared.workflow.script

import com.sos.jobscheduler.data.workflow.Workflow.EndNode
import com.sos.jobscheduler.data.workflow.WorkflowScript.{End, ForkJoin, Job, OnError}
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
    val script = WorkflowScript(Job(A.id, AAgentPath, TestJobPath) :: Nil)
    assert(toWorkflowRoute(script) ==
      WorkflowRoute(start = A.id, nodes = A :: Nil, transitions = Nil))
  }

  "WorkflowScript of multiple jobs" in {
    val script = WorkflowScript(List(
      Job(A.id, AAgentPath, TestJobPath),
      Job(D.id, AAgentPath, TestJobPath),
      Job(G.id, AAgentPath, TestJobPath)))
    assert(toWorkflowRoute(script) ==
      WorkflowRoute(start = A.id,
        nodes = List(A, D, G),
        transitions = List(
          Transition(A.id, D.id, ForwardTransition),
          Transition(D.id, G.id, ForwardTransition))))
  }

  "WorkflowScript with OnError" in {
    val errorNode = EndNode(NodeId("ERROR"))
    val script = WorkflowScript(List(
      Job(A.id, AAgentPath, TestJobPath),
      OnError(errorNode.id),
      Job(D.id, AAgentPath, TestJobPath),
      End(END.id),
      End(errorNode.id)))
    assert(toWorkflowRoute(script) ==
      WorkflowRoute(start = A.id,
        nodes = List(A, D, END, errorNode),
        transitions = List(
          Transition(from = List(A.id), to = List(D.id, errorNode.id), SuccessErrorTransition),
          Transition(D.id, END.id, ForwardTransition))))
  }

  "Workflow from TestForkSetting" in {
    val script = WorkflowScript(List(
      Job(A.id, AAgentPath, TestJobPath),
      ForkJoin(ListMap(
        WorkflowRoute.Id("🥕") → WorkflowScript(List(
          Job(Bx.id, AAgentPath, TestJobPath),
          Job(Cx.id, AAgentPath, TestJobPath))),
        WorkflowRoute.Id("🍋") → WorkflowScript(List(
          Job(By.id, AAgentPath, TestJobPath),
          Job(Cy.id, BAgentPath, TestJobPath))))),
      Job(D.id, AAgentPath, TestJobPath),
      ForkJoin(ListMap(
        WorkflowRoute.Id("🥕") → WorkflowScript(List(
          Job(Ex.id, AAgentPath, TestJobPath),
          Job(Fx.id, AAgentPath, TestJobPath))),
        WorkflowRoute.Id("🍋") → WorkflowScript(List(
          Job(Ey.id, AAgentPath, TestJobPath),
          Job(Fy.id, AAgentPath, TestJobPath))))),
      Job(G.id, AAgentPath, TestJobPath),
      End(END.id)))
    assert(toWorkflowRoute(script) == TestWorkflow.route)
  }
}
