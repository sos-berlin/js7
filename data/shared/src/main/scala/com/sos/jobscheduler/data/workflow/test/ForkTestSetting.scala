package com.sos.jobscheduler.data.workflow.test

import com.sos.jobscheduler.data.agent.AgentRefPath
import com.sos.jobscheduler.data.job.ExecutablePath
import com.sos.jobscheduler.data.workflow.instructions.executable.WorkflowJob
import com.sos.jobscheduler.data.workflow.instructions.{Execute, Fork}
import com.sos.jobscheduler.data.workflow.{Workflow, WorkflowPath}

/**
  * @author Joacim Zschimmer
  */
object ForkTestSetting {

  val AAgentRefPath = AgentRefPath("/AGENT-A")
  val BAgentRefPath = AgentRefPath("/AGENT-B")
  val AgentRefPaths = List(AAgentRefPath, BAgentRefPath)
  val TestExecutablePath = ExecutablePath("/executable.cmd")
  val AJobName = WorkflowJob.Name("A")
  val BJobName = WorkflowJob.Name("B")
  val AJob = WorkflowJob(AAgentRefPath, TestExecutablePath)
  val BJob = WorkflowJob(BAgentRefPath, TestExecutablePath)
  val AExecute = Execute.Named(AJobName)
  val BExecute = Execute.Named(BJobName)

  val TestWorkflowSource = """
   |define workflow {
   |  // First instruction is a fork: Event OrderStarted here
   |  fork(
   |    "🥕" { job A },
   |    "🍋" { job A });
   |  fork(
   |    "🥕" { job A },
   |    "🍋" { job A });
   |  job B;
   |  fork(
   |    "🥕" { job B },
   |    "🍋" { job A; job B });
   |  fork(
   |    "🥕" { job A },
   |    "🍋" { job B });
   |
   |  define job A {
   |    execute executable="/executable.cmd", agent="AGENT-A"
   |  }
   |  define job B {
   |    execute executable="/executable.cmd", agent="AGENT-B"
   |  }
   |}
   """.stripMargin.trim

  val TestWorkflow = Workflow(
    WorkflowPath("/WORKFLOW") % "INITIAL" ,
    Vector(
      /*0*/ Fork.of(
        "🥕" -> Workflow.of(AExecute),
        "🍋" -> Workflow.of(AExecute)),
      /*1*/ Fork.of(
        "🥕" -> Workflow.of(AExecute),
        "🍋" -> Workflow.of(AExecute)),
      /*2*/ BExecute,
      /*3*/ Fork.of(
        "🥕" -> Workflow.of(BExecute),
        "🍋" -> Workflow.of(AExecute, BExecute)),
      /*4*/ Fork.of(
        "🥕" -> Workflow.of(AExecute),
        "🍋" -> Workflow.of(BExecute))),
    Map(
      AJobName -> AJob,
      BJobName -> BJob),
    source = Some(TestWorkflowSource/*Must be the source source of this workflow*/))
  //  🥕   🍋
  //  Bx   By
  //  Cx   Cy   ⟵ Cy runs on BAgentRefPath
  //     D
  //  Ex   Ey
  //  Fx   Fy
  //     G
  //  Hx   Hy   ⟵ Hy runs on BAgentRefPath
  //  Ix   Iy   ⟵ Iy runs on BAgentRefPath
  //     J
  //    END
}
