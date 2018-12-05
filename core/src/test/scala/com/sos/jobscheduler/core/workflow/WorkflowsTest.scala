package com.sos.jobscheduler.core.workflow

import com.sos.jobscheduler.core.workflow.Workflows.ExecutableWorkflow
import com.sos.jobscheduler.data.workflow.instructions.{Fork, Gap}
import com.sos.jobscheduler.data.workflow.position.Position
import com.sos.jobscheduler.data.workflow.test.ForkTestSetting._
import com.sos.jobscheduler.data.workflow.{Workflow, WorkflowPath}
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class WorkflowsTest extends FreeSpec {

  "reduceForAgent A" in {
    assert(TestWorkflow.reduceForAgent(AAgentPath) == Workflow(
      TestWorkflow.id,
      Vector(
        /*0*/ Fork.of(
          "🥕" → Workflow.of(AExecute),
          "🍋" → Workflow.of(AExecute)),
        /*1*/ Fork.of(
          "🥕" → Workflow.of(AExecute),
          "🍋" → Workflow.of(AExecute)),
        /*2*/ Gap,
        /*3*/ Fork.of(
          "🥕" → Workflow.of(Gap),
          "🍋" → Workflow.of(AExecute, Gap)),
        /*4*/ Fork.of(
          "🥕" → Workflow.of(AExecute),
          "🍋" → Workflow.of(Gap))),
      Map(
        AJobName → AJob,
        BJobName → BJob), // TODO May be deleted, too
      source = TestWorkflow.source))
  }

  "reduceForAgent B" in {
    assert(TestWorkflow.reduceForAgent(BAgentPath) == Workflow(
      WorkflowPath("/WORKFLOW") % "(initial)" ,
      Vector(
        /*0*/ Gap,
        /*1*/ Gap,
        /*2*/ BExecute,
        /*3*/ Fork.of(
          "🥕" → Workflow.of(BExecute),
          "🍋" → Workflow.of(Gap, BExecute)),
        /*4*/ Fork.of(
          "🥕" → Workflow.of(Gap),
          "🍋" → Workflow.of(BExecute))),
      Map(
        AJobName → AJob,  // TODO May be deleted, too
        BJobName → BJob),
      source = TestWorkflow.source))
  }

  "isStartableOnAgent" - {
    val isStartableSetting = List(
      Position(0) → List(AAgentPath),
      Position(0, "🥕", 0) → List(AAgentPath),
      Position(0, "🥕", 1) → Nil,
      Position(0, "🍋", 0) → List(AAgentPath),
      Position(0, "🍋", 1) → Nil,
      Position(1) → List(AAgentPath),
      Position(1, "🥕", 0) → List(AAgentPath),
      Position(1, "🥕", 1) → Nil,
      Position(1, "🍋", 0) → List(AAgentPath),
      Position(1, "🍋", 1) → Nil,
      Position(2) → List(BAgentPath),
      Position(3) → List(AAgentPath, BAgentPath),
      Position(3, "🥕", 0) → List(BAgentPath),
      Position(3, "🥕", 1) → Nil,
      Position(3, "🍋", 0) → List(AAgentPath),
      Position(3, "🍋", 1) → List(BAgentPath),
      Position(3, "🍋", 2) → Nil,
      Position(4) → List(AAgentPath, BAgentPath),  // Order 🍋 is created on A but executed on B
      Position(4, "🥕", 0) → List(AAgentPath),
      Position(4, "🥕", 1) → Nil,
      Position(4, "🍋", 0) → List(BAgentPath),
      Position(4, "🍋", 1) → Nil,
      Position(5) → Nil)

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

  //"determinedExecutingAgent" - {
  //  val setting = List(
  //    Position(0) → Some(AAgentPath),
  //    Position(1) → Some(AAgentPath),
  //    Position(2) → Some(BAgentPath),
  //    Position(3) → None,
  //    Position(4) → None,
  //    Position(5) → Nil)
  //
  //  for ((position, expected) ← setting) {
  //    s"determinedExecutingAgent($position)" in {
  //      assert(TestWorkflow.determinedExecutingAgent(position) == expected)
  //    }
  //  }
  //}
}
