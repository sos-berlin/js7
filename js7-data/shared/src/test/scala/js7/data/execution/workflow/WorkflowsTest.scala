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
    assert(TestWorkflow.reduceForAgent(AAgentName) == Workflow(
      TestWorkflow.id,
      Vector(
        /*0*/ Fork.of(
          "🥕" -> Workflow.of(AExecute),
          "🍋" -> Workflow.of(AExecute)),
        /*1*/ Fork.of(
          "🥕" -> Workflow.of(AExecute),
          "🍋" -> Workflow.of(AExecute)),
        /*2*/ Gap(),
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
    assert(TestWorkflow.reduceForAgent(BAgentName) == Workflow(
      WorkflowPath("/WORKFLOW") ~ "INITIAL" ,
      Vector(
        /*0*/ Gap(),
        /*1*/ Gap(),
        /*2*/ BExecute,
        /*3*/ Fork.of(
          "🥕" -> Workflow.of(BExecute),
          "🍋" -> Workflow.of(Gap(), BExecute)),
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
      Position(0) -> List(AAgentName),
      Position(0) / "fork+🥕" % 0 -> List(AAgentName),
      Position(0) / "fork+🥕" % 1 -> Nil,
      Position(0) / "fork+🍋" % 0 -> List(AAgentName),
      Position(0) / "fork+🍋" % 1 -> Nil,
      Position(1) -> List(AAgentName),
      Position(1) / "fork+🥕" % 0 -> List(AAgentName),
      Position(1) / "fork+🥕" % 1 -> Nil,
      Position(1) / "fork+🍋" % 0 -> List(AAgentName),
      Position(1) / "fork+🍋" % 1 -> Nil,
      Position(2) -> List(BAgentName),
      Position(3) -> List(AAgentName, BAgentName),
      Position(3) / "fork+🥕" % 0 -> List(BAgentName),
      Position(3) / "fork+🥕" % 1 -> Nil,
      Position(3) / "fork+🍋" % 0 -> List(AAgentName),
      Position(3) / "fork+🍋" % 1 -> List(BAgentName),
      Position(3) / "fork+🍋" % 2 -> Nil,
      Position(4) -> List(AAgentName, BAgentName),  // Order 🍋 is created on A but executed on B
      Position(4) / "fork+🥕" % 0 -> List(AAgentName),
      Position(4) / "fork+🥕" % 1 -> Nil,
      Position(4) / "fork+🍋" % 0 -> List(BAgentName),
      Position(4) / "fork+🍋" % 1 -> Nil,
      Position(5) -> Nil)

    for ((position, agentNames) <- isStartableSetting) {
      for ((agentName, expected) <- agentNames.map(_ -> true) ++ (AgentNames filterNot agentNames.toSet).map(_ -> false)) {
        s"isStartableOnAgent($position $agentName) = $expected" in {
          assert(TestWorkflow.isStartableOnAgent(position, agentName) == expected)
        }
        s".reduceForAgent.isStartableOnAgent($position $agentName) = $expected" in {
          //assert(SimpleTestWorkflow.reduceForAgent(agentName).isStartableOnAgent(position, agentName))
          assert(TestWorkflow.reduceForAgent(agentName).isStartableOnAgent(position, agentName) == expected)
        }
      }
    }
  }

  //"determinedExecutingAgent" - {
  //  val setting = List(
  //    Position(0) -> Some(AAgentName),
  //    Position(1) -> Some(AAgentName),
  //    Position(2) -> Some(BAgentName),
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
