package com.sos.jobscheduler.data.workflow

import cats.data.Validated.Valid
import cats.syntax.show._
import com.sos.jobscheduler.data.agent.AgentRefPath
import com.sos.jobscheduler.data.job.{ExecutablePath, ExecutableScript}
import com.sos.jobscheduler.data.order.OrderId
import com.sos.jobscheduler.data.source.SourcePos
import com.sos.jobscheduler.data.workflow.WorkflowPrinter.WorkflowShow
import com.sos.jobscheduler.data.workflow.instructions.executable.WorkflowJob
import com.sos.jobscheduler.data.workflow.instructions.expr.Expression.{BooleanConstant, Equal, In, ListExpression, NumericConstant, Or, OrderReturnCode, StringConstant, Variable}
import com.sos.jobscheduler.data.workflow.instructions.{AwaitOrder, Execute, ExplicitEnd, Fork, Goto, If, IfNonZeroReturnCodeGoto, ImplicitEnd, Offer, ReturnCodeMeaning}
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
          Execute.Anonymous(WorkflowJob(AgentRefPath("/AGENT"), ExecutablePath("/my-script")),
            sourcePos(20, 67)),
          ImplicitEnd(sourcePos(69, 70)))),
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
          Execute.Anonymous(WorkflowJob(AgentRefPath("/AGENT"), ExecutablePath("/my-script"), Map("KEY" -> "VALUE")),
            sourcePos(20, 95)),
          ImplicitEnd(sourcePos(97, 98)))),
      """define workflow {
        |  execute agent="/AGENT", arguments={"KEY": "VALUE"}, executable="/my-script";
        |}
        |""".stripMargin)
  }

  "execute successReturnCodes=(), script" in {
    check(
      Workflow(
        WorkflowPath.NoId,
        Vector(
          // FIXME quotes at string start or end does not work!
          Execute.Anonymous(WorkflowJob(AgentRefPath("/AGENT"), ExecutableScript("LINE 1\n'''LINE 2''' "), Map("KEY" -> "VALUE"), ReturnCodeMeaning.Success.of(0, 1)),
            sourcePos(20, 134)),
          ImplicitEnd(sourcePos(136, 137)))),
      """define workflow {
        |  execute agent="/AGENT", arguments={"KEY": "VALUE"}, successReturnCodes=[0, 1], script=''''LINE 1
        |'''LINE 2''' '''';
        |}
        |""".stripMargin)
  }

  "execute failureReturnCodes=()" in {
    check(
      Workflow(
        WorkflowPath.NoId,
        Vector(
          Execute.Anonymous(WorkflowJob(AgentRefPath("/AGENT"), ExecutablePath("/my-script"), Map("KEY" -> "VALUE"), ReturnCodeMeaning.NoFailure),
            sourcePos(20, 118)),
          ImplicitEnd(sourcePos(120, 121)))),
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
          Execute.Named(WorkflowJob.Name("A"), sourcePos = sourcePos(20, 25)),
          Execute.Named(WorkflowJob.Name("B"), sourcePos = sourcePos(29, 34)),
          ImplicitEnd(sourcePos(236, 237))),
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
          "A" @: Execute.Anonymous(WorkflowJob(AgentRefPath("/AGENT"), ExecutablePath("/EXECUTABLE")),
            sourcePos(23, 71)),
          ImplicitEnd(sourcePos(73, 74)))),
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
              In(OrderReturnCode, ListExpression(NumericConstant(1) :: NumericConstant(2) :: Nil)),
              Equal(Variable(StringConstant("KEY")), StringConstant("VALUE"))),
            Workflow.of(
              Execute.Anonymous(WorkflowJob(AgentRefPath("/AGENT"), ExecutablePath("/EXECUTABLE")),
                sourcePos(73, 121)),
              ImplicitEnd(sourcePos(125, 126))),
            sourcePos = sourcePos(20, 66)),
          ImplicitEnd(sourcePos(127, 128)))),
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
          If(Equal(OrderReturnCode, NumericConstant(-1)),
            Workflow.of(
              Execute.Anonymous(WorkflowJob(AgentRefPath("/AGENT"), ExecutablePath("/A-THEN")),
                sourcePos(48, 92)),
              If(BooleanConstant(true),
                Workflow.of(
                  Execute.Anonymous(WorkflowJob(AgentRefPath("/AGENT"), ExecutablePath("/B-THEN")),
                    sourcePos(116, 160)),
                  ImplicitEnd(sourcePos(166, 167))),
                Some(Workflow.of(
                  Execute.Anonymous(WorkflowJob(AgentRefPath("/AGENT"), ExecutablePath("/B-ELSE")),
                    sourcePos(181, 225)),
                  ImplicitEnd(sourcePos(231, 232)))),
                sourcePos(98, 107)),
              ImplicitEnd(sourcePos(235, 236))),
            sourcePos = sourcePos(20, 41)),
          ImplicitEnd(sourcePos(237, 238)))),
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
              Execute.Anonymous(WorkflowJob(AgentRefPath("/AGENT"), ExecutablePath("/A")),
                sourcePos(43+1, 82+1)),
              ImplicitEnd(sourcePos(88+1, 89+1)))),
            Fork.Branch("🍋", Workflow.of(
              Execute.Anonymous(WorkflowJob(AgentRefPath("/AGENT"), ExecutablePath("/B")),
                sourcePos(107+2, 146+2)),
              ImplicitEnd(sourcePos(152+2, 153+2))))),
          sourcePos(20, 24)),
        ImplicitEnd(sourcePos(156+2, 157+2))),
      """define workflow {
        |  fork (
        |    "🥕" {
        |      execute agent="/AGENT", executable="/A";
        |    },
        |    "🍋" {
        |      execute agent="/AGENT", executable="/B";
        |    });
        |}
        |""".stripMargin)
  }

  "offer" in {
    check(
      Workflow(WorkflowPath.NoId, Vector(
        Offer(OrderId("OFFERED"), 60.seconds, sourcePos(20, 55)),
        ImplicitEnd(sourcePos(57, 58)))),
      """define workflow {
        |  offer orderId="OFFERED", timeout=60;
        |}
        |""".stripMargin)
  }

  "await" in {
    check(
      Workflow(WorkflowPath.NoId, Vector(
        AwaitOrder(OrderId("OFFERED"), sourcePos(20, 43)),
        ImplicitEnd(sourcePos(45, 46)))),
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
          Execute.Anonymous(WorkflowJob(AgentRefPath("/AGENT"), ExecutablePath("/A")), sourcePos(20, 59)),
          IfNonZeroReturnCodeGoto(Label("FAILURE"), sourcePos(63, 94)),
          Execute.Anonymous(WorkflowJob(AgentRefPath("/AGENT"), ExecutablePath("/B")), sourcePos(98, 137)),
          Goto(Label("END"), sourcePos(141, 149)),
          "FAILURE" @:
          Execute.Anonymous(WorkflowJob(AgentRefPath("/AGENT"), ExecutablePath("/OnFailure")), sourcePos(162, 209)),
          "END" @:
          ExplicitEnd(sourcePos(218, 221)))),
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

  private def sourcePos(start: Int, end: Int) = Some(SourcePos(start, end))

  private def check(workflow: Workflow, source: String): Unit = {
    assert(workflow.show == source)
    assert(WorkflowParser.parse(source) == Valid(workflow.copy(source = Some(source))))
  }
}

