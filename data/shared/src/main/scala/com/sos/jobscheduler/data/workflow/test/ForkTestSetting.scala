package com.sos.jobscheduler.data.workflow.test

import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.workflow.instructions.{ForkJoin, Job}
import com.sos.jobscheduler.data.workflow.{JobPath, Workflow, WorkflowPath}

/**
  * @author Joacim Zschimmer
  */
object ForkTestSetting {

  val AAgentPath = AgentPath("/AGENT-A")
  val BAgentPath = AgentPath("/AGENT-B")
  val AgentPaths = List(AAgentPath, BAgentPath)
  val AJob = Job(JobPath("/JOB"), AAgentPath)
  val BJob = Job(JobPath("/JOB"), BAgentPath)
  val TestJobPath = JobPath("/JOB")

  val TestWorkflowNotation = """
    |job "JOB" on "AGENT-A";
    |fork(
    |  "🥕" { job "JOB" on "AGENT-A"; job "JOB" on "AGENT-A"; },
    |  "🍋" { job "JOB" on "AGENT-A"; job "JOB" on "AGENT-B"; });
    |job "JOB" on "AGENT-A";
    |fork(
    |  "🥕" { job "JOB" on "AGENT-A"; job "JOB" on "AGENT-A"; },
    |  "🍋" { job "JOB" on "AGENT-A"; job "JOB" on "AGENT-A"; });
    |job "JOB" on "AGENT-A";
    |fork(
    |  "🥕" { job "JOB" on "AGENT-A"; job "JOB" on "AGENT-A"; },
    |  "🍋" { job "JOB" on "AGENT-B"; job "JOB" on "AGENT-B"; });
    |job "JOB" on "AGENT-A";
    """.stripMargin.trim

  val TestWorkflow = Workflow(
    WorkflowPath("/WORKFLOW"),
    Vector(
      /*0*/ AJob,
      /*1*/ ForkJoin.of(
        "🥕" → Workflow.of(AJob, AJob),
        "🍋" → Workflow.of(AJob, BJob)),
      /*2*/ AJob,
      /*3*/ ForkJoin.of(
        "🥕" → Workflow.of(AJob, AJob),
        "🍋" → Workflow.of(AJob, AJob)),
      /*4*/ AJob,
      /*5*/ ForkJoin.of(
        "🥕" → Workflow.of(AJob, AJob),
        "🍋" → Workflow.of(BJob, BJob)),
      /*6*/ AJob),
    source = Some(TestWorkflowNotation/*Must be the source source of this workflow*/))
  //     A
  //  🥕   🍋
  //  Bx   By
  //  Cx   Cy   ⟵ Cy runs on BAgentPath
  //     D
  //  Ex   Ey
  //  Fx   Fy
  //     G
  //  Hx   Hy   ⟵ Hy runs on BAgentPath
  //  Ix   Iy   ⟵ Iy runs on BAgentPath
  //     J
  //    END
}
