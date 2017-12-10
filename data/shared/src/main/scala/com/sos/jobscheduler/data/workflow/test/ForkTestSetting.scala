package com.sos.jobscheduler.data.workflow.test

import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.workflow.Workflow.{EndNode, JobNode}
import com.sos.jobscheduler.data.workflow.transition.Transition
import com.sos.jobscheduler.data.workflow.transition.TransitionType.Outlet
import com.sos.jobscheduler.data.workflow.transitions.{ForkTransition, JoinTransition}
import com.sos.jobscheduler.data.workflow.{JobPath, NodeId, Workflow, WorkflowPath}

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

  val XOutletId = Outlet.Id("🥕")
  val YOutletId = Outlet.Id("🍋")

  val (a, c) = Transition.forkJoin(forkNode = A, joinNode = D, Vector(XOutletId → Bx, YOutletId → By), Vector(Cx, Cy), ForkTransition, JoinTransition)
  val bx = Transition(Bx, Cx)
  val by = Transition(By, Cy)
  val (d, f) = Transition.forkJoin(forkNode = D, joinNode = G, Vector(XOutletId → Ex, YOutletId → Ey), Vector(Fx, Fy), ForkTransition, JoinTransition)
  val ex = Transition(Ex, Fx)
  val ey = Transition(Ey, Fy)
  val g = Transition(G, END)

  val TestWorkflow = Workflow(WorkflowPath("/WORKFLOW"), A.id, Vector(a, bx, by, c, d, ex, ey, f, g))
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
