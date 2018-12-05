package com.sos.jobscheduler.data.workflow

import cats.data.Validated.Valid
import cats.syntax.show._
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.job.ExecutablePath
import com.sos.jobscheduler.data.order.OrderId
import com.sos.jobscheduler.data.workflow.WorkflowPrinter.WorkflowShow
import com.sos.jobscheduler.data.workflow.instructions.executable.WorkflowJob
import com.sos.jobscheduler.data.workflow.instructions.expr.Expression.{BooleanConstant, Equal, In, ListExpression, NumericConstant, Or, OrderReturnCode, StringConstant, Variable}
import com.sos.jobscheduler.data.workflow.instructions.{AwaitOrder, Execute, ExplicitEnd, Fork, Goto, If, IfNonZeroReturnCodeGoto, Offer, ReturnCodeMeaning}
import com.sos.jobscheduler.data.workflow.parser.WorkflowParser
import org.scalatest.FreeSpec
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
final class WorkflowPrinterTest extends FreeSpec {
  // Also tested by WorkflowParserTest.

  "execute" in {
    check(
      Workflow(
        WorkflowPath.NoId,
        Vector(
          Execute(WorkflowJob(AgentPath("/AGENT"), ExecutablePath("/my-script"))))),
      """define workflow {
        |  execute executable="/my-script", agent="/AGENT";
        |}
        |""".stripMargin)
  }

  "execute defaultArguments" in {
    check(
      Workflow(
        WorkflowPath.NoId,
        Vector(
          Execute(WorkflowJob(AgentPath("/AGENT"), ExecutablePath("/my-script"), Map("KEY" → "VALUE"))))),
      """define workflow {
        |  execute executable="/my-script", agent="/AGENT", arguments={"KEY": "VALUE"};
        |}
        |""".stripMargin)
  }

  "execute successReturnCodes=()" in {
    check(
      Workflow(
        WorkflowPath.NoId,
        Vector(
          Execute(WorkflowJob(AgentPath("/AGENT"), ExecutablePath("/my-script"), Map("KEY" → "VALUE"), ReturnCodeMeaning.Success.of(0, 1))))),
      """define workflow {
        |  execute executable="/my-script", agent="/AGENT", arguments={"KEY": "VALUE"}, successReturnCodes=[0, 1];
        |}
        |""".stripMargin)
  }

  "execute failureReturnCodes=()" in {
    check(
      Workflow(
        WorkflowPath.NoId,
        Vector(
          Execute(WorkflowJob(AgentPath("/AGENT"), ExecutablePath("/my-script"), Map("KEY" → "VALUE"), ReturnCodeMeaning.NoFailure)))),
      """define workflow {
        |  execute executable="/my-script", agent="/AGENT", arguments={"KEY": "VALUE"}, failureReturnCodes=[];
        |}
        |""".stripMargin)
  }

  "job JOB" in {
    check(
      Workflow(
        WorkflowPath.NoId,
        Vector(
          Execute(WorkflowJob.Name("A")),
          Execute(WorkflowJob.Name("B"))),
        Map(
          WorkflowJob.Name("A") → WorkflowJob(AgentPath("/AGENT"), ExecutablePath("/a-script"), Map("KEY" → "VALUE"), ReturnCodeMeaning.Success.of(0, 1)),
          WorkflowJob.Name("B") → WorkflowJob(AgentPath("/AGENT"), ExecutablePath("/b-script")))),
      """define workflow {
        |  job A;
        |  job B;
        |
        |  define job A {
        |    execute executable="/a-script", agent="/AGENT", arguments={"KEY": "VALUE"}, successReturnCodes=[0, 1]
        |  }
        |  define job B {
        |    execute executable="/b-script", agent="/AGENT"
        |  }
        |}
        |""".stripMargin)
  }

  "Label and single instruction" in {
    check(
      Workflow(
        WorkflowPath.NoId,
        Vector(
          "A" @: Execute(WorkflowJob(AgentPath("/AGENT"), ExecutablePath("/EXECUTABLE"))))),
      """define workflow {
        |  A: execute executable="/EXECUTABLE", agent="/AGENT";
        |}
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
            Workflow.of(Execute(WorkflowJob(AgentPath("/AGENT"), ExecutablePath("/EXECUTABLE"))))))),
      """define workflow {
        |  if ((returnCode in [1, 2]) || $KEY == 'VALUE') {
        |    execute executable="/EXECUTABLE", agent="/AGENT";
        |  }
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
              Execute(WorkflowJob(AgentPath("/AGENT"), ExecutablePath("/A-THEN"))),
              If(BooleanConstant(true),
                Workflow.of(Execute(WorkflowJob(AgentPath("/AGENT"), ExecutablePath("/B-THEN")))),
                Some(Workflow.of(Execute(WorkflowJob(AgentPath("/AGENT"), ExecutablePath("/B-ELSE")))))))))),
      """define workflow {
        |  if (returnCode == -1) {
        |    execute executable="/A-THEN", agent="/AGENT";
        |    if (true) {
        |      execute executable="/B-THEN", agent="/AGENT";
        |    } else {
        |      execute executable="/B-ELSE", agent="/AGENT";
        |    }
        |  }
        |}
        |""".stripMargin)
  }

  "fork" in {
    check(
      Workflow.of(
        Fork(Vector(
          Fork.Branch("🥕", Workflow.of(Execute(WorkflowJob(AgentPath("/AGENT"), ExecutablePath("/A"))))),
          Fork.Branch("🍋", Workflow.of(Execute(WorkflowJob(AgentPath("/AGENT"), ExecutablePath("/B")))))))),
      """define workflow {
        |  fork (
        |    "🥕" {
        |      execute executable="/A", agent="/AGENT";
        |    },
        |    "🍋" {
        |      execute executable="/B", agent="/AGENT";
        |    });
        |}
        |""".stripMargin)
  }

  "offer" in {
    check(
      Workflow(WorkflowPath.NoId, Vector(Offer(OrderId("OFFERED"), 60.seconds))),
      """define workflow {
        |  offer orderId="OFFERED", timeout=60;
        |}
        |""".stripMargin)
  }

  "await" in {
    check(
      Workflow(WorkflowPath.NoId, Vector(AwaitOrder(OrderId("OFFERED")))),
      """define workflow {
        |  await orderId="OFFERED";
        |}
        |""".stripMargin)
  }

  "onError and goto" in {
    check(
      Workflow(
        WorkflowPath.NoId,
        Vector(
          Execute(WorkflowJob(AgentPath("/AGENT"), ExecutablePath("/A"))),
          IfNonZeroReturnCodeGoto(Label("FAILURE")),
          Execute(WorkflowJob(AgentPath("/AGENT"), ExecutablePath("/B"))),
          Goto(Label("END")),
          "FAILURE" @:
          Execute(WorkflowJob(AgentPath("/AGENT"), ExecutablePath("/OnFailure"))),
          "END" @:
          ExplicitEnd)),
      """define workflow {
        |  execute executable="/A", agent="/AGENT";
        |  ifNonZeroReturnCodeGoto FAILURE;
        |  execute executable="/B", agent="/AGENT";
        |  goto END;
        |  FAILURE: execute executable="/OnFailure", agent="/AGENT";
        |  END: end;
        |}
        |""".stripMargin)
  }

  private def check(workflow: Workflow, source: String): Unit = {
    assert(workflow.show == source)
    assert(WorkflowParser.parse(source) == Valid(workflow.copy(source = Some(source))))
  }
}

