package com.sos.jobscheduler.data.workflow.parser

import cats.data.Validated.{Invalid, Valid}
import cats.syntax.show._
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.job.JobPath
import com.sos.jobscheduler.data.order.OrderId
import com.sos.jobscheduler.data.workflow.WorkflowPrinter.WorkflowShow
import com.sos.jobscheduler.data.workflow.instructions.expr.Expression.{Equal, In, ListExpression, NumericConstant, Or, OrderReturnCode, StringConstant, Variable}
import com.sos.jobscheduler.data.workflow.instructions.{AwaitOrder, ExplicitEnd, ForkJoin, Goto, If, IfNonZeroReturnCodeGoto, Job, Offer, ReturnCodeMeaning}
import com.sos.jobscheduler.data.workflow.test.ForkTestSetting.{TestWorkflow, TestWorkflowNotation}
import com.sos.jobscheduler.data.workflow.{Label, Workflow, WorkflowPath}
import org.scalatest.FreeSpec
import scala.concurrent.duration._
import scala.util.control.NoStackTrace

/**
  * @author Joacim Zschimmer
  */
final class WorkflowParserTest extends FreeSpec {

  "parse" in {
    assert(parse(TestWorkflowNotation) == TestWorkflow.copy(id = WorkflowPath.NoId))
  }

  "Single instruction with relative paths" in {
    check("""job "A" on "AGENT";""",
      Workflow(
        WorkflowPath.NoId,
        Vector(
          Job(JobPath("/A"), AgentPath("/AGENT")))))
  }

  "Single instruction with absolute job path" in {
    check("""job "A" on "AGENT";""",
      Workflow(
        WorkflowPath.NoId,
        Vector(
          Job(JobPath("/A"), AgentPath("/AGENT")))))
  }

  "job with successReturnCodes" in {
    check("""job "A" on "AGENT" successReturnCodes=(0, 1, 3);""",
      Workflow(
        WorkflowPath.NoId,
        Vector(
          Job(JobPath("/A"), AgentPath("/AGENT"), ReturnCodeMeaning.Success.of(0, 1, 3)))))
  }

  "job with failureReturnCodes" in {
    check("""job "A" on "AGENT" failureReturnCodes=(1, 3);""",
      Workflow(
        WorkflowPath.NoId,
        Vector(
          Job(JobPath("/A"), AgentPath("/AGENT"), ReturnCodeMeaning.Failure.of(1, 3)))))
  }

  "Label and single instruction" in {
    check("""A: job "A" on "AGENT";""",
      Workflow(
        WorkflowPath.NoId,
        Vector(
          "A" @: Job(JobPath("/A"), AgentPath("/AGENT")))))
  }

  "if (...)" in {
    check("""if ((returnCode in (1, 2)) || $KEY == "VALUE") { job "THEN" on "AGENT" }""",
      Workflow(
        WorkflowPath.NoId,
        Vector(
          If(
            Or(
              In(OrderReturnCode, ListExpression(NumericConstant(1) :: NumericConstant(2) :: Nil)),
              Equal(Variable(StringConstant("KEY")), StringConstant("VALUE"))),
            Workflow.of(Job(JobPath("/THEN"), AgentPath("/AGENT")))))))
  }

  "if (...) else" in {
    check("""if (returnCode == -1) { job "THEN" on "AGENT" } else { job "ELSE" on "AGENT" }""",
      Workflow(
        WorkflowPath.NoId,
        Vector(
          If(Equal(OrderReturnCode, NumericConstant(-1)),
            Workflow.of(Job(JobPath("/THEN"), AgentPath("/AGENT"))),
            Some(Workflow.of(Job(JobPath("/ELSE"), AgentPath("/AGENT"))))))))
  }

  "fork" in {
    check(
      """fork(
        |  "🥕" {
        |    job "/a" on "/agent-a";
        |  },
        |  "🍋" {
        |    job "/b" on "/agent-b";
        |  });""".stripMargin,
      Workflow.of(
        ForkJoin(Vector(
          ForkJoin.Branch("🥕", Workflow.of(Job(JobPath("/a"), AgentPath("/agent-a")))),
          ForkJoin.Branch("🍋", Workflow.of(Job(JobPath("/b"), AgentPath("/agent-b"))))))))
  }

  "offer" in {
    check("""offer orderId = "OFFERED", timeout = 60;""",
      Workflow(WorkflowPath.NoId, Vector(Offer(OrderId("OFFERED"), 60.seconds))))
  }

  "await" in {
    check("""await orderId = "OFFERED";""",
      Workflow(WorkflowPath.NoId, Vector(AwaitOrder(OrderId("OFFERED")))))
  }

  "onError and goto" in {
    check("""
      job "/A" on "/AGENT";
      ifNonZeroReturnCodeGoto FAILURE;
      job "/B" on "/AGENT";
      goto END;
      FAILURE: job "/OnFailure" on "/AGENT";
      END: end;""",
    Workflow(
      WorkflowPath.NoId,
      Vector(
        Job(JobPath("/A"), AgentPath("/AGENT")),
        IfNonZeroReturnCodeGoto(Label("FAILURE")),
        Job(JobPath("/B"), AgentPath("/AGENT")),
        Goto(Label("END")),
        "FAILURE" @:
        Job(JobPath("/OnFailure"), AgentPath("/AGENT")),
        "END" @:
        ExplicitEnd)))
  }

  //for (n ← sys.props.get("test.speed") map (_.toInt)) "Speed" - {
  //  s"Parsing $n processes" in {
  //    info(measureTime(n, "processes") {
  //      parse(TestWorkflowNotation)
  //    }.toString)
  //  }
  //
  //  s"Parsing and compiling $n processes, parallel" in {
  //    info(measureTimeParallel(n, "processes") {
  //      parse(TestWorkflowNotation)
  //    }.toString)
  //  }
  //}

  "Comments" in {
    val source = """/*comment
        */
        //comment
        /*comment/**/job/***/"A"/**/on/**/"AGENT"/**/;/**///comment
      """
    assert(parse(source) == Workflow(
      WorkflowPath.NoId,
      Vector(
        Job(JobPath("/A"), AgentPath("/AGENT"))),
      source = Some(source)))
  }

  private def check(source: String, workflow: Workflow): Unit = {
    assert(WorkflowParser.parse(source) == Valid(workflow.copy(source = Some(source))))
    assert(WorkflowParser.parse(workflow.show) == Valid(workflow.copy(source = Some(workflow.show))))
  }

  private def parse(workflowString: String): Workflow =
    WorkflowParser.parse(workflowString) match {
      case Valid(workflow) ⇒ workflow
      case Invalid(problem) ⇒ throw new AssertionError(problem.toString, problem.throwableOption.orNull) with NoStackTrace
    }
}
