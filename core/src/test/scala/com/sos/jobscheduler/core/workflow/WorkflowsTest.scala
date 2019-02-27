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
    assert(TestWorkflow.reduceForAgent(AAgentRefPath) == Workflow(
      TestWorkflow.id,
      Vector(
        /*0*/ Fork.of(
          "🥕" -> Workflow.of(AExecute),
          "🍋" -> Workflow.of(AExecute)),
        /*1*/ Fork.of(
          "🥕" -> Workflow.of(AExecute),
          "🍋" -> Workflow.of(AExecute)),
        /*2*/ Gap,
        /*3*/ Fork.of(
          "🥕" -> Workflow.of(Gap),
          "🍋" -> Workflow.of(AExecute, Gap)),
        /*4*/ Fork.of(
          "🥕" -> Workflow.of(AExecute),
          "🍋" -> Workflow.of(Gap))),
      Map(
        AJobName -> AJob,
        BJobName -> BJob), // TODO May be deleted, too
      source = TestWorkflow.source))
  }

  "reduceForAgent B" in {
    assert(TestWorkflow.reduceForAgent(BAgentRefPath) == Workflow(
      WorkflowPath("/WORKFLOW") % "INITIAL" ,
      Vector(
        /*0*/ Gap,
        /*1*/ Gap,
        /*2*/ BExecute,
        /*3*/ Fork.of(
          "🥕" -> Workflow.of(BExecute),
          "🍋" -> Workflow.of(Gap, BExecute)),
        /*4*/ Fork.of(
          "🥕" -> Workflow.of(Gap),
          "🍋" -> Workflow.of(BExecute))),
      Map(
        AJobName -> AJob,  // TODO May be deleted, too
        BJobName -> BJob),
      source = TestWorkflow.source))
  }

  "isStartableOnAgent" - {
    val isStartableSetting = List(
      Position(0) -> List(AAgentRefPath),
      Position(0) / "fork+🥕" % 0 -> List(AAgentRefPath),
      Position(0) / "fork+🥕" % 1 -> Nil,
      Position(0) / "fork+🍋" % 0 -> List(AAgentRefPath),
      Position(0) / "fork+🍋" % 1 -> Nil,
      Position(1) -> List(AAgentRefPath),
      Position(1) / "fork+🥕" % 0 -> List(AAgentRefPath),
      Position(1) / "fork+🥕" % 1 -> Nil,
      Position(1) / "fork+🍋" % 0 -> List(AAgentRefPath),
      Position(1) / "fork+🍋" % 1 -> Nil,
      Position(2) -> List(BAgentRefPath),
      Position(3) -> List(AAgentRefPath, BAgentRefPath),
      Position(3) / "fork+🥕" % 0 -> List(BAgentRefPath),
      Position(3) / "fork+🥕" % 1 -> Nil,
      Position(3) / "fork+🍋" % 0 -> List(AAgentRefPath),
      Position(3) / "fork+🍋" % 1 -> List(BAgentRefPath),
      Position(3) / "fork+🍋" % 2 -> Nil,
      Position(4) -> List(AAgentRefPath, BAgentRefPath),  // Order 🍋 is created on A but executed on B
      Position(4) / "fork+🥕" % 0 -> List(AAgentRefPath),
      Position(4) / "fork+🥕" % 1 -> Nil,
      Position(4) / "fork+🍋" % 0 -> List(BAgentRefPath),
      Position(4) / "fork+🍋" % 1 -> Nil,
      Position(5) -> Nil)

    for ((position, agentRefPaths) <- isStartableSetting) {
      for ((agentRefPath, expected) <- agentRefPaths.map(_ -> true) ++ (AgentRefPaths filterNot agentRefPaths.toSet).map(_ -> false)) {
        s"isStartableOnAgent($position $agentRefPath) = $expected" in {
          assert(TestWorkflow.isStartableOnAgent(position, agentRefPath) == expected)
        }
        s".reduceForAgent.isStartableOnAgent($position $agentRefPath) = $expected" in {
          //assert(SimpleTestWorkflow.reduceForAgent(agentRefPath).isStartableOnAgent(position, agentRefPath))
          assert(TestWorkflow.reduceForAgent(agentRefPath).isStartableOnAgent(position, agentRefPath) == expected)
        }
      }
    }
  }

  //"determinedExecutingAgent" - {
  //  val setting = List(
  //    Position(0) -> Some(AAgentRefPath),
  //    Position(1) -> Some(AAgentRefPath),
  //    Position(2) -> Some(BAgentRefPath),
  //    Position(3) -> None,
  //    Position(4) -> None,
  //    Position(5) -> Nil)
  //
  //  for ((position, expected) <- setting) {
  //    s"determinedExecutingAgent($position)" in {
  //      assert(TestWorkflow.determinedExecutingAgent(position) == expected)
  //    }
  //  }
  //}
}
