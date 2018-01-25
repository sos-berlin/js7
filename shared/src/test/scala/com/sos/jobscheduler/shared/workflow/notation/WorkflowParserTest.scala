package com.sos.jobscheduler.shared.workflow.notation

import com.sos.jobscheduler.common.time.Stopwatch.{measureTime, measureTimeParallel}
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.job.ReturnCode
import com.sos.jobscheduler.data.order.OrderId
import com.sos.jobscheduler.data.workflow.Instruction.simplify._
import com.sos.jobscheduler.data.workflow.instructions.{AwaitOrder, ExplicitEnd, Goto, IfFailedGoto, IfReturnCode, Job, Offer, ReturnCodeMeaning}
import com.sos.jobscheduler.data.workflow.test.ForkTestSetting.{TestWorkflow, TestWorkflowNotation}
import com.sos.jobscheduler.data.workflow.{JobPath, Label, Workflow}
import org.scalatest.FreeSpec
import scala.concurrent.duration._
import scala.util.control.NoStackTrace

/**
  * @author Joacim Zschimmer
  */
final class WorkflowParserTest extends FreeSpec {

  "parse" in {
    assert(parse(TestWorkflowNotation) == TestWorkflow)
  }

  "Single instruction with relative paths" in {
    val source = """job "A" on "AGENT";"""
    assert(parse(source) ==
      Workflow(
        Vector(
          Job(JobPath("/A"), AgentPath("/AGENT"))),
        Some(source)))
  }

  "Single instruction with absolute job path" in {
    val source = """job "A" on "AGENT";"""
    assert(parse(source) ==
      Workflow(
        Vector(
          Job(JobPath("/A"), AgentPath("/AGENT"))),
        Some(source)))
  }

  "job with successReturnCodes" in {
    val source = """job "A" on "AGENT" successReturnCodes=(0, 1, 3);"""
    assert(parse(source) ==
      Workflow(
        Vector(
          Job(JobPath("/A"), AgentPath("/AGENT"), ReturnCodeMeaning.Success.of(0, 1, 3))),
        Some(source)))
  }

  "job with failureReturnCodes" in {
    val source = """job "A" on "AGENT" failureReturnCodes=(1, 3);"""
    assert(parse(source) ==
      Workflow(
        Vector(
          Job(JobPath("/A"), AgentPath("/AGENT"), ReturnCodeMeaning.Failure.of(1, 3))),
        Some(source)))
  }

  "Label and single instruction" in {
    val source = """A: job "A" on "AGENT";"""
    assert(parse(source) ==
      Workflow(
        Vector(
          "A" @: Job(JobPath("/A"), AgentPath("/AGENT"))),
        Some(source)))
  }

  "if (...)" in {
    val source = """if (returnCode 1, 2, 3) { job "THEN" on "AGENT" }"""
    assert(parse(source) ==
      Workflow(
        Vector(
          IfReturnCode(List(ReturnCode(1), ReturnCode(2), ReturnCode(3)), Vector(
            Workflow.of(Job(JobPath("/THEN"), AgentPath("/AGENT")))))),
        Some(source)))
  }

  "if (...) else" in {
    val source = """if (returnCode -1) { job "THEN" on "AGENT" } else { job "ELSE" on "AGENT" }"""
    assert(parse(source) ==
      Workflow(
        Vector(
          IfReturnCode(List(ReturnCode(-1)), Vector(
            Workflow.of(Job(JobPath("/THEN"), AgentPath("/AGENT"))),
            Workflow.of(Job(JobPath("/ELSE"), AgentPath("/AGENT")))))),
        Some(source)))
  }

  "offer" in {
    val source = """offer orderId = "OFFERED", timeout = 60;"""
    assert(parse(source) == Workflow(Vector(Offer(OrderId("OFFERED"), 60.seconds)), Some(source)))
  }

  "await" in {
    val source = """await orderId = "OFFERED";"""
    assert(parse(source) == Workflow(Vector(AwaitOrder(OrderId("OFFERED"))), Some(source)))
  }

  "onError and goto" in {
    val source = """
      job "A" on "AGENT";
      ifFailed FAILURE;
      job "B" on "AGENT";
      goto END;
      FAILURE: job "OnFailure" on "AGENT";
      END: end;"""
    assert(parse(source) == Workflow(
      Vector(
        Job(JobPath("/A"), AgentPath("/AGENT")),
        IfFailedGoto(Label("FAILURE")),
        Job(JobPath("/B"), AgentPath("/AGENT")),
        Goto(Label("END")),
        "FAILURE" @:
        Job(JobPath("/OnFailure"), AgentPath("/AGENT")),
        "END" @:
        ExplicitEnd),
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
    assert(parse(source) == Workflow(
      Vector(
        Job(JobPath("/A"), AgentPath("/AGENT"))),
      source = Some(source)))
  }

  private def parse(workflowString: String): Workflow =
    WorkflowParser.parse(workflowString) match {
      case Right(workflow) ⇒ workflow
      case Left(message) ⇒ throw new AssertionError(message) with NoStackTrace
    }
}
