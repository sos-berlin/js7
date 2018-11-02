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
  val AExecutable = Execute(WorkflowJob(AAgentPath, TestExecutablePath))
  val BExecutable = Execute(WorkflowJob(BAgentPath, TestExecutablePath))

  val TestWorkflowNotation = """
   |workflow {
   |  execute executable="/executable", agent="AGENT-A";
   |  fork(
   |    "🥕" {
   |      execute executable="/executable", agent="AGENT-A";
   |      execute executable="/executable", agent="AGENT-A";
   |    },
   |    "🍋" {
   |      execute executable="/executable", agent="AGENT-A";
   |      execute executable="/executable", agent="AGENT-B";
   |    });
   |  execute executable="/executable", agent="AGENT-A";
   |  fork(
   |    "🥕" {
   |      execute executable="/executable", agent="AGENT-A";
   |      execute executable="/executable", agent="AGENT-A";
   |    },
   |    "🍋" {
   |      execute executable="/executable", agent="AGENT-A";
   |      execute executable="/executable", agent="AGENT-A";
   |    });
   |  execute executable="/executable", agent="AGENT-A";
   |  fork(
   |    "🥕" {
   |      execute executable="/executable", agent="AGENT-A";
   |      execute executable="/executable", agent="AGENT-A";
   |    },
   |    "🍋" {
   |      execute executable="/executable", agent="AGENT-B";
   |      execute executable="/executable", agent="AGENT-B";
   |    });
   |  execute executable="/executable", agent="AGENT-A";
   |}
   """.stripMargin.trim

  val TestWorkflow = Workflow(
    WorkflowPath("/WORKFLOW") % "(initial)" ,
    Vector(
      /*0*/ AExecutable,
      /*1*/ ForkJoin.of(
        "🥕" → Workflow.of(AExecutable, AExecutable),
        "🍋" → Workflow.of(AExecutable, BExecutable)),
      /*2*/ AExecutable,
      /*3*/ ForkJoin.of(
        "🥕" → Workflow.of(AExecutable, AExecutable),
        "🍋" → Workflow.of(AExecutable, AExecutable)),
      /*4*/ AExecutable,
      /*5*/ ForkJoin.of(
        "🥕" → Workflow.of(AExecutable, AExecutable),
        "🍋" → Workflow.of(BExecutable, BExecutable)),
      /*6*/ AExecutable),
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
