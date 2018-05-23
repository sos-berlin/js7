package com.sos.jobscheduler.data.workflow

import cats.data.Validated.Valid
import cats.syntax.show._
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.job.JobPath
import com.sos.jobscheduler.data.order.OrderId
import com.sos.jobscheduler.data.workflow.WorkflowPrinter.WorkflowShow
import com.sos.jobscheduler.data.workflow.instructions.expr.Expression.{BooleanConstant, Equal, In, ListExpression, NumericConstant, Or, OrderReturnCode, StringConstant, Variable}
import com.sos.jobscheduler.data.workflow.instructions.{AwaitOrder, ExplicitEnd, ForkJoin, Goto, If, IfNonZeroReturnCodeGoto, Job, Offer, ReturnCodeMeaning}
import com.sos.jobscheduler.data.workflow.parser.WorkflowParser
import org.scalatest.FreeSpec
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
final class WorkflowPrinterTest extends FreeSpec {
  // Also tested by WorkflowParserTest.

  "Single instruction with absolute job path" in {
    check(
      Workflow(
        WorkflowPath.NoId,
        Vector(
          Job(JobPath("/A"), AgentPath("/AGENT")))),
      """job "/A" on "/AGENT";
        |""".stripMargin)
  }

  "job with successReturnCodes" in {
    check(
      Workflow(
        WorkflowPath.NoId,
        Vector(
          Job(JobPath("/A"), AgentPath("/AGENT"), ReturnCodeMeaning.Success.of(0, 1, 3)))),
      """job "/A" on "/AGENT" successReturnCodes=(0, 1, 3);
        |""".stripMargin)
  }

  "job with failureReturnCodes" in {
    check(
      Workflow(
        WorkflowPath.NoId,
        Vector(
          Job(JobPath("/A"), AgentPath("/AGENT"), ReturnCodeMeaning.Failure.of(1, 3)))),
      """job "/A" on "/AGENT" failureReturnCodes=(1, 3);
        |""".stripMargin)
  }

  "Label and single instruction" in {
    check(
      Workflow(
        WorkflowPath.NoId,
        Vector(
          "A" @: Job(JobPath("/A"), AgentPath("/AGENT")))),
      """A: job "/A" on "/AGENT";
        |""".stripMargin)
  }

  "if (...)" in {
    check(
      Workflow(
        WorkflowPath.NoId,
        Vector(
          If(
            Or(
              In(OrderReturnCode, ListExpression(NumericConstant(1) :: NumericConstant(2) :: Nil)),
              Equal(Variable(StringConstant("KEY")), StringConstant("VALUE"))),
            Workflow.of(Job(JobPath("/THEN"), AgentPath("/AGENT")))))),
      """if ((returnCode in (1, 2)) || $KEY == 'VALUE') {
        |  job "/THEN" on "/AGENT";
        |}
        |""".stripMargin)
  }

  "if (...) else" in {
    check(
      Workflow(
        WorkflowPath.NoId,
        Vector(
          If(Equal(OrderReturnCode, NumericConstant(-1)),
            Workflow.of(
              Job(JobPath("/THEN-1"), AgentPath("/AGENT")),
              If(BooleanConstant(true),
                Workflow.of(Job(JobPath("/THEN-2"), AgentPath("/AGENT"))),
                Some(Workflow.of(Job(JobPath("/ELSE-2"), AgentPath("/AGENT"))))))))),
      """if (returnCode == -1) {
        |  job "/THEN-1" on "/AGENT";
        |  if (true) {
        |    job "/THEN-2" on "/AGENT";
        |  } else {
        |    job "/ELSE-2" on "/AGENT";
        |  }
        |}
        |""".stripMargin)
  }

  "fork" in {
    check(
      Workflow.of(
        ForkJoin(Vector(
          ForkJoin.Branch("🥕", Workflow.of(Job(JobPath("/a"), AgentPath("/agent-a")))),
          ForkJoin.Branch("🍋", Workflow.of(Job(JobPath("/b"), AgentPath("/agent-b"))))))),
      """fork (
        |  "🥕" {
        |    job "/a" on "/agent-a";
        |  },
        |  "🍋" {
        |    job "/b" on "/agent-b";
        |  });
        |""".stripMargin)
  }

  "offer" in {
    check(
      Workflow(WorkflowPath.NoId, Vector(Offer(OrderId("OFFERED"), 60.seconds))),
      """offer orderId="OFFERED", timeout=60;
        |""".stripMargin)
  }

  "await" in {
    check(
      Workflow(WorkflowPath.NoId, Vector(AwaitOrder(OrderId("OFFERED")))),
      """await orderId="OFFERED";
        |""".stripMargin)
  }

  "onError and goto" in {
    check(
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
          ExplicitEnd)),
      """job "/A" on "/AGENT";
        |ifNonZeroReturnCodeGoto FAILURE;
        |job "/B" on "/AGENT";
        |goto END;
        |FAILURE: job "/OnFailure" on "/AGENT";
        |END: end;
        |""".stripMargin)
  }

  private def check(workflow: Workflow, source: String): Unit = {
    assert(workflow.show == source)
    assert(WorkflowParser.parse(source) == Valid(workflow.copy(source = Some(source))))
  }
}

