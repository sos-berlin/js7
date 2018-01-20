package com.sos.jobscheduler.shared.workflow.notation

import com.sos.jobscheduler.common.time.Stopwatch.{measureTime, measureTimeParallel}
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.job.ReturnCode
import com.sos.jobscheduler.data.workflow.Instruction.simplify._
import com.sos.jobscheduler.data.workflow.Instruction.{Goto, IfErrorGoto, IfReturnCode, Job}
import com.sos.jobscheduler.data.workflow.test.ForkTestSetting.{TestWorkflow, TestWorkflowNotation}
import com.sos.jobscheduler.data.workflow.{AgentJobPath, Instruction, JobPath, Label, Workflow}
import org.scalatest.FreeSpec
import scala.util.control.NoStackTrace

/**
  * @author Joacim Zschimmer
  */
final class WorkflowParserTest extends FreeSpec {

  "parse" in {
    assert(parse(TestWorkflowNotation) == TestWorkflow)
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

  "if (...)" in {
    val source = """if (returnCode 1, 2, 3) { job "THEN" on "AGENT" }"""
    assert(parse(source) ==
      Workflow(
        Vector(
          IfReturnCode(List(ReturnCode(1), ReturnCode(2), ReturnCode(3)), Vector(
            Workflow.of(Job(AgentJobPath(AgentPath("/AGENT"), JobPath("/THEN"))))))),
        Some(source)))
  }

  "if (...) else" in {
    val source = """if (returnCode -1) { job "THEN" on "AGENT" } else { job "ELSE" on "AGENT" }"""
    assert(parse(source) ==
      Workflow(
        Vector(
          IfReturnCode(List(ReturnCode(-1)), Vector(
            Workflow.of(Job(AgentJobPath(AgentPath("/AGENT"), JobPath("/THEN")))),
            Workflow.of(Job(AgentJobPath(AgentPath("/AGENT"), JobPath("/ELSE"))))))),
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
        IfErrorGoto(Label("ERROR")),
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
        parse(TestWorkflowNotation)
      }.toString)
    }

    s"Parsing and compiling $n processes, parallel" in {
      info(measureTimeParallel(n, "processes") {
        parse(TestWorkflowNotation)
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
