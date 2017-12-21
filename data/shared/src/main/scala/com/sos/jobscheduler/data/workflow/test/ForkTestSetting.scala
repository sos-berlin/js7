package com.sos.jobscheduler.data.workflow.test

import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.workflow.Workflow.{EndNode, JobNode}
import com.sos.jobscheduler.data.workflow.WorkflowScript.{End, ForkJoin, Job}
import com.sos.jobscheduler.data.workflow.transition.Transition
import com.sos.jobscheduler.data.workflow.transitions.{ForkTransition, JoinTransition}
import com.sos.jobscheduler.data.workflow.{AgentJobPath, JobPath, NodeId, Workflow, WorkflowPath, WorkflowGraph, WorkflowScript}
import scala.collection.immutable.{List, ListMap}

/**
  * @author Joacim Zschimmer
  */
object ForkTestSetting {

  val AAgentPath = AgentPath("/AGENT-A")
  val BAgentPath = AgentPath("/AGENT-B")
  val AAgentJobPath = AgentJobPath(AAgentPath, JobPath("/JOB"))
  val BAgentJobPath = AgentJobPath(BAgentPath, JobPath("/JOB"))
  val TestJobPath = JobPath("/JOB")

  val A  = JobNode(NodeId("A" ), AAgentJobPath)
  val Bx = JobNode(NodeId("Bx"), AAgentJobPath)
  val By = JobNode(NodeId("By"), AAgentJobPath)
  val Cx = JobNode(NodeId("Cx"), AAgentJobPath)
  val Cy = JobNode(NodeId("Cy"), BAgentJobPath)
  val D  = JobNode(NodeId("D" ), AAgentJobPath)
  val Ex = JobNode(NodeId("Ex"), AAgentJobPath)
  val Ey = JobNode(NodeId("Ey"), AAgentJobPath)
  val Fx = JobNode(NodeId("Fx"), AAgentJobPath)
  val Fy = JobNode(NodeId("Fy"), AAgentJobPath)
  val G  = JobNode(NodeId("G" ), AAgentJobPath)
  val END = EndNode(NodeId("END"))

  val TestWorkflowScript = WorkflowScript(List(
    Job(A.id, AAgentJobPath),
    ForkJoin(ListMap(
      WorkflowGraph.Id("🥕") → WorkflowScript(List(
        Job(Bx.id, AAgentJobPath),
        Job(Cx.id, AAgentJobPath))),
      WorkflowGraph.Id("🍋") → WorkflowScript(List(
        Job(By.id, AAgentJobPath),
        Job(Cy.id, BAgentJobPath))))),
    Job(D.id, AAgentJobPath),
    ForkJoin(ListMap(
      WorkflowGraph.Id("🥕") → WorkflowScript(List(
        Job(Ex.id, AAgentJobPath),
        Job(Fx.id, AAgentJobPath))),
      WorkflowGraph.Id("🍋") → WorkflowScript(List(
        Job(Ey.id, AAgentJobPath),
        Job(Fy.id, AAgentJobPath))))),
    Job(G.id, AAgentJobPath),
    End(END.id)))

  val bx = Transition(Bx.id, Cx.id)
  val by = Transition(By.id, Cy.id)
  private val bChildGraphs = ListMap(
    WorkflowGraph.Id("🥕") → WorkflowGraph(start = Bx.id, nodes = List(Bx, Cx), transitions = List(bx)),
    WorkflowGraph.Id("🍋") → WorkflowGraph(start = By.id, nodes = List(By, Cy), transitions = List(by)))

  val ex = Transition(Ex.id, Fx.id)
  val ey = Transition(Ey.id, Fy.id)
  private val eChildGraphs = ListMap(
    WorkflowGraph.Id("🥕") → WorkflowGraph(start = Ex.id, nodes = List(Ex, Fx), transitions = List(ex)),
    WorkflowGraph.Id("🍋") → WorkflowGraph(start = Ey.id, nodes = List(Ey, Fy), transitions = List(ey)))

  val (a, c) = Transition.forkJoin(forkNodeId = A.id, joinNodeId = D.id, bChildGraphs, ForkTransition, JoinTransition)
  val (d, f) = Transition.forkJoin(forkNodeId = D.id, joinNodeId = G.id, eChildGraphs, ForkTransition, JoinTransition)
  val g = Transition(G.id, END.id)

  val TestWorkflow = Workflow(WorkflowPath("/WORKFLOW"), A.id,
    Vector(A, Bx, Cx, By, Cy, D, Ex, Fx, Ey, Fy, G, END),
    Vector(a, c, d, f, g))
  //     A
  //  🥕   🍋
  //  Bx   By
  //  Cx   Cy   ⟵ Cy runs on BAgentJobPath
  //     D
  //  Ex   Ey
  //  Fx   Fy
  //     G
  //    END
}
