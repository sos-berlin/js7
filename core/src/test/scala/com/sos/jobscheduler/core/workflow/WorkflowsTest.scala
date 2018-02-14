package com.sos.jobscheduler.core.workflow

import com.sos.jobscheduler.core.workflow.Workflows.ExecutableWorkflow
import com.sos.jobscheduler.data.workflow.Instruction.simplify._
import com.sos.jobscheduler.data.workflow.instructions.{ForkJoin, Gap}
import com.sos.jobscheduler.data.workflow.test.ForkTestSetting
import com.sos.jobscheduler.data.workflow.test.ForkTestSetting._
import com.sos.jobscheduler.data.workflow.{Position, Workflow}
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class WorkflowsTest extends FreeSpec {

  "reduceForAgent A" in {
    assert(ForkTestSetting.TestWorkflow.reduceForAgent(AAgentPath) == Workflow(
      Vector(
        AJob,
        ForkJoin.of(
          "🥕" → Workflow.of(AJob, AJob),
          "🍋" → Workflow.of(AJob, Gap)),
        AJob,
        ForkJoin.of(
          "🥕" → Workflow.of(AJob, AJob),
          "🍋" → Workflow.of(AJob, AJob)),
        AJob,
        ForkJoin.of(
          "🥕" → Workflow.of(AJob, AJob),
          "🍋" → Workflow.of(Gap               , Gap)),
        AJob),
      source = None))
  }

  "reduceForAgent B" in {
    assert(ForkTestSetting.TestWorkflow.reduceForAgent(BAgentPath) == Workflow(
      Vector(
        /*0*/ Gap,
        /*1*/ ForkJoin.of(
                "🥕" → Workflow.of(Gap, Gap),
                "🍋" → Workflow.of(Gap, BJob)),
        /*2*/ Gap,
        /*3*/ Gap,
        /*4*/ Gap,
        /*5*/ ForkJoin.of(
                "🥕" → Workflow.of(Gap, Gap),
                "🍋" → Workflow.of(BJob, BJob)),
        /*6*/ Gap),
      source = None))
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
          //assert(SimpleTestWorkflow.workflow.reduceForAgent(agentPath).isStartableOnAgent(position, agentPath))
          assert(TestWorkflow.reduceForAgent(agentPath).isStartableOnAgent(position, agentPath) == expected)
        }
      }
    }
  }
}
