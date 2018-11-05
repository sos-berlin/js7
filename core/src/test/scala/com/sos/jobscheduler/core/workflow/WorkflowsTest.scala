package com.sos.jobscheduler.core.workflow

import com.sos.jobscheduler.core.workflow.Workflows.ExecutableWorkflow
import com.sos.jobscheduler.data.workflow.Workflow
import com.sos.jobscheduler.data.workflow.instructions.{Execute, ForkJoin, Gap}
import com.sos.jobscheduler.data.workflow.position.Position
import com.sos.jobscheduler.data.workflow.test.ForkTestSetting._
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class WorkflowsTest extends FreeSpec {

  "reduceForAgent A" in {
    assert(TestWorkflow.reduceForAgent(AAgentPath) == Workflow(
      TestWorkflow.id,
      Vector(
        AExecute,
        ForkJoin.of(
          "🥕" → Workflow.of(AExecute, Execute.Named(AJobName)),
          "🍋" → Workflow.of(AExecute, Gap)),
        AExecute,
        ForkJoin.of(
          "🥕" → Workflow.of(AExecute, Execute.Named(AJobName)),
          "🍋" → Workflow.of(AExecute, Execute.Named(AJobName))),
        AExecute,
        ForkJoin.of(
          "🥕" → Workflow.of(AExecute, Execute.Named(AJobName)),
          "🍋" → Workflow.of(Gap, Gap)),
        Execute.Named(AJobName)),
      Map(
        AJobName → AJob,
        BJobName → BJob), // TODO May be deleted, too
      source = TestWorkflow.source))
  }

  "reduceForAgent B" in {
    assert(TestWorkflow.reduceForAgent(BAgentPath) == Workflow(
      TestWorkflow.id,
      Vector(
        /*0*/ Gap,
        /*1*/ ForkJoin.of(
                "🥕" → Workflow.of(Gap, Gap),
                "🍋" → Workflow.of(Gap, Execute.Named(BJobName))),
        /*2*/ Gap,
        /*3*/ Gap,
        /*4*/ Gap,
        /*5*/ ForkJoin.of(
                "🥕" → Workflow.of(Gap, Gap),
                "🍋" → Workflow.of(BExecute, Execute.Named(BJobName))),
        /*6*/ Gap),
      Map(
        AJobName → AJob,  // TODO May be deleted, too
        BJobName → BJob),
      source = TestWorkflow.source))
  }

  "isStartableOnAgent" - {
    val isStartableSetting = List(
      Position(0) → List(AAgentPath),
      Position(1) → List(AAgentPath),
      Position(1, "🥕", 0) → List(AAgentPath),
      Position(1, "🥕", 1) → List(AAgentPath),
      Position(1, "🍋", 0) → List(AAgentPath),
      Position(1, "🍋", 1) → List(BAgentPath),
      Position(2) → List(AAgentPath),
      Position(3) → List(AAgentPath),
      Position(3, "🥕", 0) → List(AAgentPath),
      Position(3, "🥕", 1) → List(AAgentPath),
      Position(3, "🍋", 0) → List(AAgentPath),
      Position(3, "🍋", 1) → List(AAgentPath),
      Position(4) → List(AAgentPath),
      Position(5) → List(AAgentPath, BAgentPath),  // Order 🍋 is created on A but executed on B
      Position(5, "🥕", 0) → List(AAgentPath),
      Position(5, "🥕", 1) → List(AAgentPath),
      Position(5, "🍋", 0) → List(BAgentPath),
      Position(5, "🍋", 1) → List(BAgentPath),
      Position(6) → List(AAgentPath),
      Position(7) → Nil)

    for ((position, agentPaths) ← isStartableSetting) {
      for ((agentPath, expected) ← agentPaths.map(_ → true) ++ (AgentPaths filterNot agentPaths.toSet).map(_ → false)) {
        s"isStartableOnAgent($position $agentPath) = $expected" in {
          assert(TestWorkflow.isStartableOnAgent(position, agentPath) == expected)
        }
        s".reduceForAgent.isStartableOnAgent($position $agentPath) = $expected" in {
          //assert(SimpleTestWorkflow.reduceForAgent(agentPath).isStartableOnAgent(position, agentPath))
          assert(TestWorkflow.reduceForAgent(agentPath).isStartableOnAgent(position, agentPath) == expected)
        }
      }
    }
  }
}
