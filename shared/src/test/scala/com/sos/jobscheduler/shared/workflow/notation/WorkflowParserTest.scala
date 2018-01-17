package com.sos.jobscheduler.shared.workflow.notation

import com.sos.jobscheduler.common.time.Stopwatch.{measureTime, measureTimeParallel}
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.workflow.Instruction.{Goto, IfError, Job}
import com.sos.jobscheduler.data.workflow.test.ForkTestSetting.{TestWorkflow, TestWorkflowScriptNotation}
import com.sos.jobscheduler.data.workflow.{AgentJobPath, Instruction, JobPath, Label, Workflow}
import org.scalatest.FreeSpec
import scala.util.control.NoStackTrace

/**
  * @author Joacim Zschimmer
  */
final class WorkflowParserTest extends FreeSpec {

  "parse" in {
    assert(parse(TestWorkflowScriptNotation) == TestWorkflow)
  }

  private val singleJobScript = Workflow(Vector(
    Job(AgentJobPath(AgentPath("/AGENT"), JobPath("/A")))))

  "Single instruction with relative paths" in {
    val source = """job "A" on "AGENT";"""
    assert(parse(source) ==
      Workflow(
        Vector(
          Job(AgentJobPath(AgentPath("/AGENT"), JobPath("/A")))),
        Some(source)))
  }

  "Single instruction with absolute job path" in {
    val source = """job "A" on "AGENT";"""
    assert(parse(source) ==
      Workflow(
        Vector(
          Job(AgentJobPath(AgentPath("/AGENT"), JobPath("/A")))),
        Some(source)))
  }

  "Label and single instruction" in {
    val source = """A: job "A" on "AGENT";"""
    assert(parse(source) ==
      Workflow(
        Vector(
          "A" @: Job(AgentJobPath(AgentPath("/AGENT"), JobPath("/A")))),
        Some(source)))
  }

  "onError and goto" in {
    val source = """
      job "A" on "AGENT";
      ifError ERROR;
      job "B" on "AGENT";
      goto END;
      ERROR: job "Error" on "AGENT";
      END: end;"""
    assert(parse(source) == Workflow(
      Vector(
        Job(AgentJobPath(AgentPath("/AGENT"), JobPath("/A"))),
        IfError(Label("ERROR")),
        Job(AgentJobPath(AgentPath("/AGENT"), JobPath("/B"))),
        Goto(Label("END")),
        "ERROR" @:
        Job(AgentJobPath(AgentPath("/AGENT"), JobPath("/Error"))),
        "END" @:
        Instruction.ExplicitEnd),
      Some(source)))
  }

  for (n ← sys.props.get("test.speed") map (_.toInt)) "Speed" - {
    s"Parsing $n processes" in {
      info(measureTime(n, "processes") {
        parse(TestWorkflowScriptNotation)
      }.toString)
    }

    s"Parsing and compiling $n processes, parallel" in {
      info(measureTimeParallel(n, "processes") {
        parse(TestWorkflowScriptNotation)
      }.toString)
    }
  }

  "Comments" in {
    val source = """/*comment
        */
        //comment
        /*comment/**/job/***/"A"/**/on/**/"AGENT"/**/;/**///comment
      """
    assert(parse(source) == singleJobScript.copy(source = Some(source)))
  }

  private def parse(workflowString: String): Workflow =
    WorkflowParser.parse(workflowString) match {
      case Right(workflow) ⇒ workflow
      case Left(message) ⇒ throw new AssertionError(message) with NoStackTrace
    }
}
