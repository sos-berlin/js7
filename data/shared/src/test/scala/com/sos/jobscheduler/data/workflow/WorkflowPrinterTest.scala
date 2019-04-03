package com.sos.jobscheduler.data.workflow

import cats.data.Validated.Valid
import cats.syntax.show._
import com.sos.jobscheduler.data.agent.AgentRefPath
import com.sos.jobscheduler.data.expression.Expression.{BooleanConstant, Equal, In, LastReturnCode, ListExpression, NamedValue, NumericConstant, Or, StringConstant}
import com.sos.jobscheduler.data.job.{ExecutablePath, ExecutableScript}
import com.sos.jobscheduler.data.order.OrderId
import com.sos.jobscheduler.data.workflow.WorkflowPrinter.WorkflowShow
import com.sos.jobscheduler.data.workflow.instructions.executable.WorkflowJob
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
          Execute.Anonymous(WorkflowJob(AgentRefPath("/AGENT"), ExecutablePath("/my-script"))))),
      """define workflow {
        |  execute agent="/AGENT", executable="/my-script";
        |}
        |""".stripMargin)
  }

  "execute defaultArguments" in {
    check(
      Workflow(
        WorkflowPath.NoId,
        Vector(
          Execute.Anonymous(WorkflowJob(AgentRefPath("/AGENT"), ExecutablePath("/my-script"), Map("KEY" -> "VALUE"))))),
      """define workflow {
        |  execute agent="/AGENT", arguments={"KEY": "VALUE"}, executable="/my-script";
        |}
        |""".stripMargin)
  }

  "Newline in string (not string expression)" in {
    check(
      Workflow(
        WorkflowPath.NoId,
        Vector(
          Execute.Anonymous(WorkflowJob(AgentRefPath("/AGENT"), ExecutablePath("/my-script"), Map("KEY\n\"$" -> "VALUE"))))),
      """define workflow {
        |  execute agent="/AGENT", arguments={"KEY\n\"\$": "VALUE"}, executable="/my-script";
        |}
        |""".stripMargin)
  }

  "execute successReturnCodes=(), script" in {
    check(
      Workflow(
        WorkflowPath.NoId,
        Vector(
          Execute.Anonymous(
            WorkflowJob(AgentRefPath("/AGENT"),
              ExecutableScript("LINE 1\nLINE 2\n'''LINE 3'''\n"),
              Map("KEY" -> "VALUE"),
              ReturnCodeMeaning.Success.of(0, 1))))),
      """define workflow {
        |  execute agent="/AGENT", arguments={"KEY": "VALUE"}, successReturnCodes=[0, 1], script=
        |''''LINE 1
        |   |LINE 2
        |   |'''LINE 3'''
        |   |''''.stripMargin;
        |}
        |""".stripMargin)
  }

  "execute failureReturnCodes=()" in {
    check(
      Workflow(
        WorkflowPath.NoId,
        Vector(
          Execute.Anonymous(WorkflowJob(AgentRefPath("/AGENT"), ExecutablePath("/my-script"), Map("KEY" -> "VALUE"), ReturnCodeMeaning.NoFailure)))),
      """define workflow {
        |  execute agent="/AGENT", arguments={"KEY": "VALUE"}, failureReturnCodes=[], executable="/my-script";
        |}
        |""".stripMargin)
  }

  "job JOB" in {
    check(
      Workflow(
        WorkflowPath.NoId,
        Vector(
          Execute.Named(WorkflowJob.Name("A")),
          Execute.Named(WorkflowJob.Name("B"))),
        Map(
          WorkflowJob.Name("A") -> WorkflowJob(AgentRefPath("/AGENT"), ExecutablePath("/a-script"), Map("KEY" -> "VALUE"), ReturnCodeMeaning.Success.of(0, 1)),
          WorkflowJob.Name("B") -> WorkflowJob(AgentRefPath("/AGENT"), ExecutablePath("/b-script")))),
      """define workflow {
        |  job A;
        |  job B;
        |
        |  define job A {
        |    execute agent="/AGENT", arguments={"KEY": "VALUE"}, successReturnCodes=[0, 1], executable="/a-script"
        |  }
        |  define job B {
        |    execute agent="/AGENT", executable="/b-script"
        |  }
        |}
        |""".stripMargin)
  }

  "Label and single instruction" in {
    check(
      Workflow(
        WorkflowPath.NoId,
        Vector(
          "A" @: Execute.Anonymous(WorkflowJob(AgentRefPath("/AGENT"), ExecutablePath("/EXECUTABLE"))))),
      """define workflow {
        |  A: execute agent="/AGENT", executable="/EXECUTABLE";
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
              In(LastReturnCode, ListExpression(NumericConstant(1) :: NumericConstant(2) :: Nil)),
              Equal(NamedValue.last("KEY"), StringConstant("VALUE"))),
            Workflow.of(
              Execute.Anonymous(WorkflowJob(AgentRefPath("/AGENT"), ExecutablePath("/EXECUTABLE"))))))),
      """define workflow {
        |  if ((returnCode in [1, 2]) || $KEY == 'VALUE') {
        |    execute agent="/AGENT", executable="/EXECUTABLE";
        |  }
        |}
        |""".stripMargin)
  }

  "if (...) else" in {
    check(
      Workflow(
        WorkflowPath.NoId,
        Vector(
          If(Equal(LastReturnCode, NumericConstant(-1)),
            Workflow.of(
              Execute.Anonymous(WorkflowJob(AgentRefPath("/AGENT"), ExecutablePath("/A-THEN"))),
              If(BooleanConstant(true),
                Workflow.of(
                  Execute.Anonymous(WorkflowJob(AgentRefPath("/AGENT"), ExecutablePath("/B-THEN")))),
                Some(Workflow.of(
                  Execute.Anonymous(WorkflowJob(AgentRefPath("/AGENT"), ExecutablePath("/B-ELSE")))))))))),
      """define workflow {
        |  if (returnCode == -1) {
        |    execute agent="/AGENT", executable="/A-THEN";
        |    if (true) {
        |      execute agent="/AGENT", executable="/B-THEN";
        |    } else {
        |      execute agent="/AGENT", executable="/B-ELSE";
        |    }
        |  }
        |}
        |""".stripMargin)
  }

  "fork" in {
    check(
      Workflow.of(
        Fork(
          Vector(
            Fork.Branch("🥕", Workflow.of(
              Execute.Anonymous(WorkflowJob(AgentRefPath("/AGENT"), ExecutablePath("/A"))))),
            Fork.Branch("🍋", Workflow.of(
              Execute.Anonymous(WorkflowJob(AgentRefPath("/AGENT"), ExecutablePath("/B")))))))),
      """define workflow {
        |  fork {
        |    "🥕": {
        |      execute agent="/AGENT", executable="/A";
        |    },
        |    "🍋": {
        |      execute agent="/AGENT", executable="/B";
        |    }
        |  };
        |}
        |""".stripMargin)
  }

  "offer" in {
    check(
      Workflow(WorkflowPath.NoId, Vector(
        Offer(OrderId("OFFERED"), 60.seconds))),
      """define workflow {
        |  offer orderId="OFFERED", timeout=60;
        |}
        |""".stripMargin)
  }

  "await" in {
    check(
      Workflow(WorkflowPath.NoId, Vector(
        AwaitOrder(OrderId("OFFERED")))),
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
          Execute.Anonymous(WorkflowJob(AgentRefPath("/AGENT"), ExecutablePath("/A"))),
          IfNonZeroReturnCodeGoto(Label("FAILURE")),
          Execute.Anonymous(WorkflowJob(AgentRefPath("/AGENT"), ExecutablePath("/B"))),
          Goto(Label("END")),
          "FAILURE" @:
          Execute.Anonymous(WorkflowJob(AgentRefPath("/AGENT"), ExecutablePath("/OnFailure"))),
          "END" @:
          ExplicitEnd())),
      """define workflow {
        |  execute agent="/AGENT", executable="/A";
        |  ifNonZeroReturnCodeGoto FAILURE;
        |  execute agent="/AGENT", executable="/B";
        |  goto END;
        |  FAILURE: execute agent="/AGENT", executable="/OnFailure";
        |  END: end;
        |}
        |""".stripMargin)
  }

  private def check(workflow: Workflow, source: String): Unit = {
    assert(workflow.show == source)
    assert(WorkflowParser.parse(source).map(_.withoutSourcePos) == Valid(workflow.copy(source = Some(source)).withoutSourcePos))
  }
}

