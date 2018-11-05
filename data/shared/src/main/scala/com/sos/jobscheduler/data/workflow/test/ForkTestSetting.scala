package com.sos.jobscheduler.data.workflow.test

import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.job.ExecutablePath
import com.sos.jobscheduler.data.workflow.instructions.executable.WorkflowJob
import com.sos.jobscheduler.data.workflow.instructions.{Execute, ForkJoin}
import com.sos.jobscheduler.data.workflow.{Workflow, WorkflowPath}

/**
  * @author Joacim Zschimmer
  */
object ForkTestSetting {

  val AAgentPath = AgentPath("/AGENT-A")
  val BAgentPath = AgentPath("/AGENT-B")
  val AgentPaths = List(AAgentPath, BAgentPath)
  val TestExecutablePath = ExecutablePath("/executable")
  val AJobName = WorkflowJob.Name("A")
  val BJobName = WorkflowJob.Name("B")
  val AJob = WorkflowJob(AAgentPath, TestExecutablePath)
  val BJob = WorkflowJob(BAgentPath, TestExecutablePath)
  val AExecute = Execute(AJob)
  val BExecute = Execute(BJob)

  val TestWorkflowSource = """
   |workflow {
   |  execute executable="/executable", agent="AGENT-A";
   |  fork(
   |    "🥕" {
   |      execute executable="/executable", agent="AGENT-A";
   |      job A;
   |    },
   |    "🍋" {
   |      execute executable="/executable", agent="AGENT-A";
   |      job B;
   |    });
   |  execute executable="/executable", agent="AGENT-A";
   |  fork(
   |    "🥕" {
   |      execute executable="/executable", agent="AGENT-A";
   |      job A;
   |    },
   |    "🍋" {
   |      execute executable="/executable", agent="AGENT-A";
   |      job A;
   |    });
   |  execute executable="/executable", agent="AGENT-A";
   |  fork(
   |    "🥕" {
   |      execute executable="/executable", agent="AGENT-A";
   |      job A;
   |    },
   |    "🍋" {
   |      execute executable="/executable", agent="AGENT-B";
   |      job B;
   |    });
   |  job A;
   |
   |  define job A {
   |    execute executable="/executable", agent="AGENT-A"
   |  }
   |  define job B {
   |    execute executable="/executable", agent="AGENT-B"
   |  }
   |}
   """.stripMargin.trim

  val TestWorkflow = Workflow(
    WorkflowPath("/WORKFLOW") % "(initial)" ,
    Vector(
      /*0*/ AExecute,
      /*1*/ ForkJoin.of(
        "🥕" → Workflow.of(AExecute, Execute.Named(AJobName)),
        "🍋" → Workflow.of(AExecute, Execute.Named(BJobName))),
      /*2*/ AExecute,
      /*3*/ ForkJoin.of(
        "🥕" → Workflow.of(AExecute, Execute.Named(AJobName)),
        "🍋" → Workflow.of(AExecute, Execute.Named(AJobName))),
      /*4*/ AExecute,
      /*5*/ ForkJoin.of(
        "🥕" → Workflow.of(AExecute, Execute.Named(AJobName)),
        "🍋" → Workflow.of(BExecute, Execute.Named(BJobName))),
      /*6*/ Execute.Named(AJobName)),
    Map(
      AJobName → AJob,
      BJobName → BJob),
    source = Some(TestWorkflowSource/*Must be the source source of this workflow*/))
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
