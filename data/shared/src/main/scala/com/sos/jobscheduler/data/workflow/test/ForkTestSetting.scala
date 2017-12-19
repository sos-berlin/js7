package com.sos.jobscheduler.data.workflow.test

import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.workflow.Workflow.{EndNode, JobNode}
import com.sos.jobscheduler.data.workflow.WorkflowScript.{End, ForkJoin, Job}
import com.sos.jobscheduler.data.workflow.transition.Transition
import com.sos.jobscheduler.data.workflow.transitions.{ForkTransition, JoinTransition}
import com.sos.jobscheduler.data.workflow.{JobPath, NodeId, Workflow, WorkflowPath, WorkflowRoute, WorkflowScript}
import scala.collection.immutable.{List, ListMap}

/**
  * @author Joacim Zschimmer
  */
object ForkTestSetting {

  val AAgentPath = AgentPath("/AGENT-A")
  val BAgentPath = AgentPath("/AGENT-B")
  val TestJobPath = JobPath("/JOB")

  val A  = JobNode(NodeId("A" ), AAgentPath, TestJobPath)
  val Bx = JobNode(NodeId("Bx"), AAgentPath, TestJobPath)
  val By = JobNode(NodeId("By"), AAgentPath, TestJobPath)
  val Cx = JobNode(NodeId("Cx"), AAgentPath, TestJobPath)
  val Cy = JobNode(NodeId("Cy"), BAgentPath, TestJobPath)
  val D  = JobNode(NodeId("D" ), AAgentPath, TestJobPath)
  val Ex = JobNode(NodeId("Ex"), AAgentPath, TestJobPath)
  val Ey = JobNode(NodeId("Ey"), AAgentPath, TestJobPath)
  val Fx = JobNode(NodeId("Fx"), AAgentPath, TestJobPath)
  val Fy = JobNode(NodeId("Fy"), AAgentPath, TestJobPath)
  val G  = JobNode(NodeId("G" ), AAgentPath, TestJobPath)
  val END = EndNode(NodeId("END"))

  val TestWorkflowScript = WorkflowScript(List(
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

  val bx = Transition(Bx.id, Cx.id)
  val by = Transition(By.id, Cy.id)
  private val bChildRoutes = ListMap(
    WorkflowRoute.Id("🥕") → WorkflowRoute(start = Bx.id, nodes = List(Bx, Cx), transitions = List(bx)),
    WorkflowRoute.Id("🍋") → WorkflowRoute(start = By.id, nodes = List(By, Cy), transitions = List(by)))

  val ex = Transition(Ex.id, Fx.id)
  val ey = Transition(Ey.id, Fy.id)
  private val eChildRoutes = ListMap(
    WorkflowRoute.Id("🥕") → WorkflowRoute(start = Ex.id, nodes = List(Ex, Fx), transitions = List(ex)),
    WorkflowRoute.Id("🍋") → WorkflowRoute(start = Ey.id, nodes = List(Ey, Fy), transitions = List(ey)))

  val (a, c) = Transition.forkJoin(forkNodeId = A.id, joinNodeId = D.id, bChildRoutes, ForkTransition, JoinTransition)
  val (d, f) = Transition.forkJoin(forkNodeId = D.id, joinNodeId = G.id, eChildRoutes, ForkTransition, JoinTransition)
  val g = Transition(G.id, END.id)

  val TestWorkflow = Workflow(WorkflowPath("/WORKFLOW"), A.id, END.id,
    Vector(A, Bx, Cx, By, Cy, D, Ex, Fx, Ey, Fy, G, END),
    Vector(a, c, d, f, g))
  //     A
  //  🥕   🍋
  //  Bx   By
  //  Cx   Cy   ⟵ Cy runs on BAgentPath
  //     D
  //  Ex   Ey
  //  Fx   Fy
  //     G
  //    END
}
