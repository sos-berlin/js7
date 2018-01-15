package com.sos.jobscheduler.shared.workflow.notation

import com.sos.jobscheduler.common.time.Stopwatch.{measureTime, measureTimeParallel}
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.workflow.Instruction.{Goto, IfError, Job}
import com.sos.jobscheduler.data.workflow.test.ForkTestSetting.{TestWorkflowScript, TestWorkflowScriptNotation}
import com.sos.jobscheduler.data.workflow.{AgentJobPath, Instruction, JobPath, Label, Workflow}
import org.scalatest.FreeSpec
import scala.util.control.NoStackTrace

/**
  * @author Joacim Zschimmer
  */
final class WorkflowParserTest extends FreeSpec {

  "parse" in {
    assert(parse(TestWorkflowScriptNotation) == TestWorkflowScript)
  }

  private val singleJobScript = Workflow(Vector(
    Job(AgentJobPath(AgentPath("/AGENT"), JobPath("/A")))))

  "Single instruction" in {
    val source = """job /A at /AGENT;"""
    assert(parse(source) ==
      Workflow(
        Vector(
          Job(AgentJobPath(AgentPath("/AGENT"), JobPath("/A")))),
        Some(source)))
  }

  "Label and single instruction" in {
    val source = """A: job /A at /AGENT;"""
    assert(parse(source) ==
      Workflow(
        Vector(
          "A" @: Job(AgentJobPath(AgentPath("/AGENT"), JobPath("/A")))),
        Some(source)))
  }

  "onError and goto" in {
    val source = """
      job /A at /AGENT;
      ifError ERROR;
      job /B at /AGENT;
      goto END;
      ERROR: job /Error at /AGENT;
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
        /*comment/**/job /***//A/**/at/**//AGENT/**/;/**///comment
      """
    assert(parse(source) == singleJobScript.copy(source = Some(source)))
  }

  private def parse(workflow: String): Workflow =
    WorkflowParser.parse(workflow) match {
      case Right(workflowScript) ⇒ workflowScript
      case Left(message) ⇒ throw new AssertionError(message) with NoStackTrace
    }
}
