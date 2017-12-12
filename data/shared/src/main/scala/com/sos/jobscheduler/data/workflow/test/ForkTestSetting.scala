package com.sos.jobscheduler.data.workflow.test

import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.workflow.Workflow.{EndNode, JobNode}
import com.sos.jobscheduler.data.workflow.transition.Transition
import com.sos.jobscheduler.data.workflow.transitions.{ForkTransition, JoinTransition}
import com.sos.jobscheduler.data.workflow.{JobPath, NodeId, Workflow, WorkflowPath, WorkflowRoute}

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

  val bx = Transition(Bx.id, Cx.id)
  val by = Transition(By.id, Cy.id)
  val bxRoute = WorkflowRoute(WorkflowRoute.Id("🥕"), start = Bx.id, end = Cx.id, nodes = List(Bx, Cx), transitions = List(bx))
  val byRoute = WorkflowRoute(WorkflowRoute.Id("🍋"), start = By.id, end = Cy.id, nodes = List(By, Cy), transitions = List(by))

  val ex = Transition(Ex.id, Fx.id)
  val ey = Transition(Ey.id, Fy.id)
  val exRoute = WorkflowRoute(WorkflowRoute.Id("🥕"), start = Ex.id, end = Fx.id, nodes = List(Ex, Fx), transitions = List(bx))
  val eyRoute = WorkflowRoute(WorkflowRoute.Id("🍋"), start = Ey.id, end = Fy.id, nodes = List(Ey, Fy), transitions = List(by))

  val (a, c) = Transition.forkJoin(forkNodeId = A.id, joinNodeId = D.id, Vector(bxRoute, byRoute), Vector(Cx.id, Cy.id), ForkTransition, JoinTransition)
  val (d, f) = Transition.forkJoin(forkNodeId = D.id, joinNodeId = G.id, Vector(exRoute, eyRoute), Vector(Fx.id, Fy.id), ForkTransition, JoinTransition)
  val g = Transition(G.id, END.id)

  val TestWorkflow = Workflow(WorkflowPath("/WORKFLOW"), A.id, END.id,
    Vector(A, Bx, By, Cx, Cy, D, Ex, Ey, Fx, Fy, G, END),
    Vector(a, bx, by, c, d, ex, ey, f, g))
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
