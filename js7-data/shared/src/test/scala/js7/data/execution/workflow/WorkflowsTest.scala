package js7.data.execution.workflow

import js7.data.execution.workflow.Workflows.ExecutableWorkflow
import js7.data.workflow.instructions.{Fork, Gap}
import js7.data.workflow.position.Position
import js7.data.workflow.test.ForkTestSetting._
import js7.data.workflow.{Workflow, WorkflowPath}
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class WorkflowsTest extends AnyFreeSpec {

  "reduceForAgent A" in {
    assert(TestWorkflow.reduceForAgent(AAgentId) == Workflow(
      TestWorkflow.id,
      Vector(
        /*0*/ Fork.of(
          "🥕" -> Workflow.of(AExecute),
          "🍋" -> Workflow.of(AExecute)),
        /*1*/ Fork.of(
          "🥕" -> Workflow.of(AExecute),
          "🍋" -> Workflow.of(AExecute)),
        /*2*/ Gap.empty,
        /*3*/ Fork.of(
          "🥕" -> Workflow.of(Gap()),
          "🍋" -> Workflow.of(AExecute, Gap())),
        /*4*/ Fork.of(
          "🥕" -> Workflow.of(AExecute),
          "🍋" -> Workflow.of(Gap()))),
      Map(
        AJobName -> AJob,
        BJobName -> BJob), // TODO May be deleted, too
      source = TestWorkflow.source))
  }

  "reduceForAgent B" in {
    assert(TestWorkflow.reduceForAgent(BAgentId) == Workflow(
      WorkflowPath("WORKFLOW") ~ "INITIAL" ,
      Vector(
        /*0*/ Gap.empty,
        /*1*/ Gap.empty,
        /*2*/ BExecute,
        /*3*/ Fork.of(
          "🥕" -> Workflow.of(BExecute),
          "🍋" -> Workflow.of(Gap.empty, BExecute)),
        /*4*/ Fork.of(
          "🥕" -> Workflow.of(Gap()),
          "🍋" -> Workflow.of(BExecute))),
      Map(
        AJobName -> AJob,  // TODO May be deleted, too
        BJobName -> BJob),
      source = TestWorkflow.source))
  }

  "isStartableOnAgent" - {
    val isStartableSetting = List(
      Position(0) -> List(AAgentId),
      Position(0) / "fork+🥕" % 0 -> List(AAgentId),
      Position(0) / "fork+🥕" % 1 -> Nil,
      Position(0) / "fork+🍋" % 0 -> List(AAgentId),
      Position(0) / "fork+🍋" % 1 -> Nil,
      Position(1) -> List(AAgentId),
      Position(1) / "fork+🥕" % 0 -> List(AAgentId),
      Position(1) / "fork+🥕" % 1 -> Nil,
      Position(1) / "fork+🍋" % 0 -> List(AAgentId),
      Position(1) / "fork+🍋" % 1 -> Nil,
      Position(2) -> List(BAgentId),
      Position(3) -> List(AAgentId, BAgentId),
      Position(3) / "fork+🥕" % 0 -> List(BAgentId),
      Position(3) / "fork+🥕" % 1 -> Nil,
      Position(3) / "fork+🍋" % 0 -> List(AAgentId),
      Position(3) / "fork+🍋" % 1 -> List(BAgentId),
      Position(3) / "fork+🍋" % 2 -> Nil,
      Position(4) -> List(AAgentId, BAgentId),  // Order 🍋 is created on A but executed on B
      Position(4) / "fork+🥕" % 0 -> List(AAgentId),
      Position(4) / "fork+🥕" % 1 -> Nil,
      Position(4) / "fork+🍋" % 0 -> List(BAgentId),
      Position(4) / "fork+🍋" % 1 -> Nil,
      Position(5) -> Nil)

    for ((position, agentIds) <- isStartableSetting) {
      for ((agentId, expected) <- agentIds.map(_ -> true) ++ (AgentIds filterNot agentIds.toSet).map(_ -> false)) {
        s"isStartableOnAgent($position $agentId) = $expected" in {
          assert(TestWorkflow.isStartableOnAgent(position, agentId) == expected)
        }
        s".reduceForAgent.isStartableOnAgent($position $agentId) = $expected" in {
          //assert(SimpleTestWorkflow.reduceForAgent(agentId).isStartableOnAgent(position, agentId))
          assert(TestWorkflow.reduceForAgent(agentId).isStartableOnAgent(position, agentId) == expected)
        }
      }
    }
  }

  //"determinedExecutingAgent" - {
  //  val setting = List(
  //    Position(0) -> Some(AAgentId),
  //    Position(1) -> Some(AAgentId),
  //    Position(2) -> Some(BAgentId),
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
