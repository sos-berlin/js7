package com.sos.jobscheduler.data.workflow.test

import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.order.OrderId
import com.sos.jobscheduler.data.workflow.Instruction.{ForkJoin, Job}
import com.sos.jobscheduler.data.workflow.{AgentJobPath, Instruction, JobPath, WorkflowPath, Workflow}
import scala.collection.immutable.ListMap

/**
  * @author Joacim Zschimmer
  */
object ForkTestSetting {

  val AAgentPath = AgentPath("/AGENT-A")
  val BAgentPath = AgentPath("/AGENT-B")
  val AgentPaths = List(AAgentPath, BAgentPath)
  val AAgentJobPath = AgentJobPath(AAgentPath, JobPath("/JOB"))
  val BAgentJobPath = AgentJobPath(BAgentPath, JobPath("/JOB"))
  val TestJobPath = JobPath("/JOB")

  val TestWorkflowScriptNotation = """
    |job /JOB on /AGENT-A;
    |fork(
    |  "🥕" { job /JOB on /AGENT-A; job /JOB on /AGENT-A; },
    |  "🍋" { job /JOB on /AGENT-A; job /JOB on /AGENT-B; });
    |job /JOB on /AGENT-A;
    |fork(
    |  "🥕" { job /JOB on /AGENT-A; job /JOB on /AGENT-A; },
    |  "🍋" { job /JOB on /AGENT-A; job /JOB on /AGENT-A; });
    |job /JOB on /AGENT-A;
    |fork(
    |  "🥕" { job /JOB on /AGENT-A; job /JOB on /AGENT-A; },
    |  "🍋" { job /JOB on /AGENT-B; job /JOB on /AGENT-B; });
    |job /JOB on /AGENT-A;
    """.stripMargin.trim

  val TestWorkflowScript = Workflow(
    Vector(
      /*0*/ Job(AAgentJobPath),
      /*1*/ ForkJoin(ListMap(
        OrderId.ChildId("🥕") → Workflow.of(Job(AAgentJobPath), Job(AAgentJobPath)),
        OrderId.ChildId("🍋") → Workflow.of(Job(AAgentJobPath), Job(BAgentJobPath)))),
      /*2*/ Job(AAgentJobPath),
      /*3*/ ForkJoin(ListMap(
        OrderId.ChildId("🥕") → Workflow.of(Job(AAgentJobPath), Job(AAgentJobPath)),
        OrderId.ChildId("🍋") → Workflow.of(Job(AAgentJobPath), Job(AAgentJobPath)))),
      /*4*/ Job(AAgentJobPath),
      /*5*/ ForkJoin(ListMap(
        OrderId.ChildId("🥕") → Workflow.of(Job(AAgentJobPath), Job(AAgentJobPath)),
        OrderId.ChildId("🍋") → Workflow.of(Job(BAgentJobPath), Job(BAgentJobPath)))),
      /*6*/ Job(AAgentJobPath)),
    source = Some(TestWorkflowScriptNotation/*Must be the source source of this workflow*/))
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

  val TestWorkflow = Workflow.Named(WorkflowPath("/WORKFLOW"), TestWorkflowScript)
}
