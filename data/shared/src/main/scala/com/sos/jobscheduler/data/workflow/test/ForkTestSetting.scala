package com.sos.jobscheduler.data.workflow.test

import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.workflow.WorkflowGraph.{EndNode, JobNode}
import com.sos.jobscheduler.data.workflow.WorkflowScript.{End, ForkJoin, Job}
import com.sos.jobscheduler.data.workflow.transition.Transition
import com.sos.jobscheduler.data.workflow.transitions.{ForkTransition, JoinTransition}
import com.sos.jobscheduler.data.workflow.{AgentJobPath, JobPath, NodeId, WorkflowGraph, WorkflowPath, WorkflowScript}
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
  val Cx = JobNode(NodeId("Cx"), AAgentJobPath)
  val By = JobNode(NodeId("By"), AAgentJobPath)
  val Cy = JobNode(NodeId("Cy"), BAgentJobPath)
  val D  = JobNode(NodeId("D" ), AAgentJobPath)
  val Ex = JobNode(NodeId("Ex"), AAgentJobPath)
  val Fx = JobNode(NodeId("Fx"), AAgentJobPath)
  val Ey = JobNode(NodeId("Ey"), AAgentJobPath)
  val Fy = JobNode(NodeId("Fy"), AAgentJobPath)
  val G  = JobNode(NodeId("G" ), AAgentJobPath)
  val Hx = JobNode(NodeId("Hx"), AAgentJobPath)
  val Ix = JobNode(NodeId("Ix"), AAgentJobPath)
  val Hy = JobNode(NodeId("Hy"), BAgentJobPath)
  val Iy = JobNode(NodeId("Iy"), BAgentJobPath)
  val J  = JobNode(NodeId("J" ), AAgentJobPath)
  val END = EndNode(NodeId("END"))

  private val bxScript = WorkflowScript(Job(Bx.id, AAgentJobPath) :: Job(Cx.id, AAgentJobPath) :: Nil)
  private val byScript = WorkflowScript(Job(By.id, AAgentJobPath) :: Job(Cy.id, BAgentJobPath) :: Nil)

  private val exScript = WorkflowScript(Job(Ex.id, AAgentJobPath) :: Job(Fx.id, AAgentJobPath) :: Nil)
  private val eyScript = WorkflowScript(Job(Ey.id, AAgentJobPath) :: Job(Fy.id, AAgentJobPath) :: Nil)

  private val hxScript = WorkflowScript(Job(Hx.id, AAgentJobPath) :: Job(Ix.id, AAgentJobPath) :: Nil)
  private val hyScript = WorkflowScript(Job(Hy.id, BAgentJobPath) :: Job(Iy.id, BAgentJobPath) :: Nil)

  val TestWorkflowScriptNotation = """
    |"A": job /JOB on /AGENT-A;
    |fork(
    |  "🥕" { "Bx": job /JOB on /AGENT-A; "Cx": job /JOB on /AGENT-A; },
    |  "🍋" { "By": job /JOB on /AGENT-A; "Cy": job /JOB on /AGENT-B; });
    |"D": job /JOB on /AGENT-A;
    |fork(
    |  "🥕" { "Ex": job /JOB on /AGENT-A; "Fx": job /JOB on /AGENT-A; },
    |  "🍋" { "Ey": job /JOB on /AGENT-A; "Fy": job /JOB on /AGENT-A; });
    |"G": job /JOB on /AGENT-A;
    |fork(
    |  "🥕" { "Hx": job /JOB on /AGENT-A; "Ix": job /JOB on /AGENT-A; },
    |  "🍋" { "Hy": job /JOB on /AGENT-B; "Iy": job /JOB on /AGENT-B; });
    |"J": job /JOB on /AGENT-A;
    |"END": end;
    """.stripMargin.trim

  val TestWorkflowScript = WorkflowScript(
    List(
      Job(A.id, AAgentJobPath),
      ForkJoin(ListMap(
        WorkflowGraph.Id("🥕") → bxScript,
        WorkflowGraph.Id("🍋") → byScript)),
      Job(D.id, AAgentJobPath),
      ForkJoin(ListMap(
        WorkflowGraph.Id("🥕") → exScript,
        WorkflowGraph.Id("🍋") → eyScript)),
      Job(G.id, AAgentJobPath),
      ForkJoin(ListMap(
        WorkflowGraph.Id("🥕") → hxScript,
        WorkflowGraph.Id("🍋") → hyScript)),
      Job(J.id, AAgentJobPath),
      End(END.id)),
    source = Some(TestWorkflowScriptNotation/*Must be the source source of this script*/))
  //     A
  //  🥕   🍋
  //  Bx   By
  //  Cx   Cy   ⟵ Cy runs on BAgentJobPath
  //     D
  //  Ex   Ey
  //  Fx   Fy
  //     G
  //  Hx   Hy   ⟵ Hy runs on BAgentJobPath
  //  Ix   Iy   ⟵ Iy runs on BAgentJobPath
  //     J
  //    END

  val bx = Transition(Bx.id, Cx.id)
  val by = Transition(By.id, Cy.id)
  private val bChildGraphs = ListMap(
    WorkflowGraph.Id("🥕") → WorkflowGraph(nodes = List(Bx, Cx), transitions = List(bx), Some(bxScript)),
    WorkflowGraph.Id("🍋") → WorkflowGraph(nodes = List(By, Cy), transitions = List(by), Some(byScript)))

  val ex = Transition(Ex.id, Fx.id)
  val ey = Transition(Ey.id, Fy.id)
  private val eChildGraphs = ListMap(
    WorkflowGraph.Id("🥕") → WorkflowGraph(nodes = List(Ex, Fx), transitions = List(ex), Some(exScript)),
    WorkflowGraph.Id("🍋") → WorkflowGraph(nodes = List(Ey, Fy), transitions = List(ey), Some(eyScript)))

  val hx = Transition(Hx.id, Ix.id)
  val hy = Transition(Hy.id, Iy.id)
  private val hChildGraphs = ListMap(
    WorkflowGraph.Id("🥕") → WorkflowGraph(nodes = List(Hx, Ix), transitions = List(hx), Some(hxScript)),
    WorkflowGraph.Id("🍋") → WorkflowGraph(nodes = List(Hy, Iy), transitions = List(hy), Some(hyScript)))

  val (a, c) = Transition.forkJoin(forkNodeId = A.id, joinNodeId = D.id, bChildGraphs, ForkTransition, JoinTransition)
  val (d, f) = Transition.forkJoin(forkNodeId = D.id, joinNodeId = G.id, eChildGraphs, ForkTransition, JoinTransition)
  val (g, i) = Transition.forkJoin(forkNodeId = G.id, joinNodeId = J.id, hChildGraphs, ForkTransition, JoinTransition)
  val j = Transition(J.id, END.id)

  val TestWorkflow = WorkflowGraph.Named(
    WorkflowPath("/WORKFLOW"),
    WorkflowGraph(
      Vector(A, Bx, Cx, By, Cy, D, Ex, Fx, Ey, Fy, G, Hx, Ix, Hy, Iy, J, END),
      Vector(a, c, d, f, g, i, j),
      originalScript = Some(TestWorkflowScript/*Must be the source source of this graph*/)))
}
