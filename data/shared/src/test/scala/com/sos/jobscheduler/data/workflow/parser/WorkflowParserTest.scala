package com.sos.jobscheduler.data.workflow.parser

import cats.data.Validated.{Invalid, Valid}
import cats.syntax.show._
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.job.ExecutablePath
import com.sos.jobscheduler.data.order.OrderId
import com.sos.jobscheduler.data.workflow.WorkflowPrinter.WorkflowShow
import com.sos.jobscheduler.data.workflow.instructions.executable.WorkflowJob
import com.sos.jobscheduler.data.workflow.instructions.expr.Expression.{Equal, In, ListExpression, NumericConstant, Or, OrderReturnCode, StringConstant, Variable}
import com.sos.jobscheduler.data.workflow.instructions.{AwaitOrder, Execute, ExplicitEnd, ForkJoin, Goto, If, IfNonZeroReturnCodeGoto, Offer, ReturnCodeMeaning}
import com.sos.jobscheduler.data.workflow.test.ForkTestSetting.{TestWorkflow, TestWorkflowSource}
import com.sos.jobscheduler.data.workflow.{Label, Workflow, WorkflowPath}
import org.scalatest.FreeSpec
import scala.concurrent.duration._
import scala.util.control.NoStackTrace

/**
  * @author Joacim Zschimmer
  */
final class WorkflowParserTest extends FreeSpec {

  "parse" in {
    assert(parse(TestWorkflowSource) == TestWorkflow.withId(WorkflowPath.NoId))
  }

  "Unknown job" in {
    val source = """
      workflow {
        if (true) {
          job A;
        }
      }"""
    assert(WorkflowParser.parse(source) == Invalid(Problem("""Unknown job 'A':6:8 ...""""")))  // TODO Wrong position in error message, should be 4:12
  }

  "Execute anonymous" in {
    check("""workflow { execute executable="/my/executable", agent="/AGENT"; }""",
      Workflow.single(
        Execute(
          WorkflowJob(AgentPath("/AGENT"), ExecutablePath("/my/executable")))))
  }

  "Execute anonymous with relative agent path" in {
    check("""workflow { execute executable="/my/executable", agent="AGENT"; }""",
      Workflow.single(
        Execute(
          WorkflowJob(AgentPath("/AGENT"), ExecutablePath("/my/executable")))))
  }

  "Execute anonymous with default arguments 'SCHEDULER_PARAM_'" in {
    check("""workflow { execute executable="/my/executable", agent="/AGENT", arguments={"A": "aaa", "B": "bbb"}, taskLimit=3; }""",
      Workflow.single(
        Execute(
          WorkflowJob(AgentPath("/AGENT"),
            ExecutablePath("/my/executable"),
            Map("A" → "aaa", "B" → "bbb"),
            taskLimit = 3))))
  }

  "Execute named" in {
    check("""
      workflow {
        job A;
        job B, arguments = { "KEY": "VALUE" };
        job C;
        define job A {
          execute executable="/my/executable", agent="/AGENT", successReturnCodes=[0, 1, 3];
        }
        define job B {
          execute executable="/my/executable", agent="/AGENT"
        }
        define job C {
          execute executable="/my/executable", agent="/AGENT"
        }
      }""",
      Workflow(
        WorkflowPath.NoId,
        Vector(
          Execute.Named(WorkflowJob.Name("A")),
          Execute.Named(WorkflowJob.Name("B"), defaultArguments = Map("KEY" → "VALUE")),
          Execute.Named(WorkflowJob.Name("C"))),
        Map(
          WorkflowJob.Name("A") →
            WorkflowJob(
              AgentPath("/AGENT"),
              ExecutablePath("/my/executable"),
              returnCodeMeaning = ReturnCodeMeaning.Success.of(0, 1, 3)),
          WorkflowJob.Name("B") →
            WorkflowJob(
              AgentPath("/AGENT"),
              ExecutablePath("/my/executable")),
          WorkflowJob.Name("C") →
            WorkflowJob(
              AgentPath("/AGENT"),
              ExecutablePath("/my/executable")))))
  }

  "Execute named with duplicate jobs" in {
    assert(WorkflowParser.parse("""
      workflow {
        job DUPLICATE;
        define job DUPLICATE {
          execute executable="/my/executable", agent="/AGENT";
        }
        define job DUPLICATE {
          execute executable="/my/executable", agent="/AGENT"
        }
      }""")
      == Invalid(Problem("""Duplicate job definitions: DUPLICATE:10:8 ...""""")))
  }

  "Single instruction with relative job path" in {
    check("""workflow { execute executable="/A", agent="AGENT"; }""",
      Workflow.anonymous(
        Vector(
          Execute(WorkflowJob(AgentPath("/AGENT"), ExecutablePath("/A"))))))
  }

  "Single instruction with absolute job path" in {
    check("""workflow { execute executable="/A", agent="/AGENT"; }""",
      Workflow.anonymous(
        Vector(
          Execute(WorkflowJob(AgentPath("/AGENT"), ExecutablePath("/A"))))))
  }

  "execute with successReturnCodes" in {
    check("""workflow { execute executable="/A", agent="AGENT", successReturnCodes=[0, 1, 3]; }""",
      Workflow.anonymous(
        Vector(
          Execute(WorkflowJob(AgentPath("/AGENT"), ExecutablePath("/A"), returnCodeMeaning = ReturnCodeMeaning.Success.of(0, 1, 3))))))
  }

  "execute with failureReturnCodes" in {
    check("""workflow { execute executable="/A", agent="AGENT", failureReturnCodes=[1, 3]; }""",
      Workflow.anonymous(
        Vector(
          Execute(WorkflowJob(AgentPath("/AGENT"), ExecutablePath("/A"), returnCodeMeaning = ReturnCodeMeaning.Failure.of(1, 3))))))
  }

  "Label and single instruction" in {
    check("""workflow { A: execute executable="/A", agent="AGENT"; }""",
      Workflow.anonymous(
        Vector(
          "A" @: Execute(WorkflowJob(AgentPath("/AGENT"), ExecutablePath("/A"))))))
  }

  "if (...)" in {
    check("""workflow { if ((returnCode in [1, 2]) || $KEY == "VALUE") { execute executable="/THEN", agent="AGENT" } }""",
      Workflow.anonymous(
        Vector(
          If(
            Or(
              In(OrderReturnCode, ListExpression(NumericConstant(1) :: NumericConstant(2) :: Nil)),
              Equal(Variable(StringConstant("KEY")), StringConstant("VALUE"))),
            Workflow.of(Execute(WorkflowJob(AgentPath("/AGENT"), ExecutablePath("/THEN"))))))))
  }

  "if (...) else" in {
    check("""workflow { if (returnCode == -1) { execute executable="/THEN", agent="AGENT" } else { execute executable="/ELSE", agent="AGENT" } }""",
      Workflow.anonymous(
        Vector(
          If(Equal(OrderReturnCode, NumericConstant(-1)),
            Workflow.of(Execute(WorkflowJob(AgentPath("/AGENT"), ExecutablePath("/THEN")))),
            Some(Workflow.of(Execute(WorkflowJob(AgentPath("/AGENT"), ExecutablePath("/ELSE")))))))))
  }

  "fork" in {
    check(
      """workflow {
        |  fork(
        |    "🥕" {
        |      execute executable="/a", agent="/agent-a";
        |    },
        |    "🍋" {
        |      execute executable="/b", agent="/agent-b";
        |    }
        |  );
        |}""".stripMargin,
      Workflow.of(
        ForkJoin(Vector(
          ForkJoin.Branch("🥕", Workflow.of(Execute(WorkflowJob(AgentPath("/agent-a"), ExecutablePath("/a"))))),
          ForkJoin.Branch("🍋", Workflow.of(Execute(WorkflowJob(AgentPath("/agent-b"), ExecutablePath("/b")))))))))
  }

  "offer" in {
    check("""workflow { offer orderId = "OFFERED", timeout = 60; }""",
      Workflow(WorkflowPath.NoId, Vector(Offer(OrderId("OFFERED"), 60.seconds))))
  }

  "await" in {
    check("""workflow { await orderId = "OFFERED"; }""",
      Workflow(WorkflowPath.NoId, Vector(AwaitOrder(OrderId("OFFERED")))))
  }

  "onError and goto" in {
    check("""
      workflow {
        execute executable="/A", agent="/AGENT";
        ifNonZeroReturnCodeGoto FAILURE;
        execute executable="/B", agent="/AGENT";
        goto END;
        FAILURE: execute executable="/OnFailure", agent="/AGENT";
        END: end;
      }""",
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
        ExplicitEnd)))
  }

  //for (n ← sys.props.get("test.speed") map (_.toInt)) "Speed" - {
  //  s"Parsing $n processes" in {
  //    info(measureTime(n, "processes") {
  //      parse(TestWorkflowSource)
  //    }.toString)
  //  }
  //
  //  s"Parsing and compiling $n processes, parallel" in {
  //    info(measureTimeParallel(n, "processes") {
  //      parse(TestWorkflowSource)
  //    }.toString)
  //  }
  //}

  "Comments" in {
    val source = """/*comment
        */
        workflow {
          //comment
          /*comment/**/execute/***/executable="/A"/**/,agent/**/=/**/"AGENT"/**/;/**///comment
        }
      """
    assert(parse(source) == Workflow(
      WorkflowPath.NoId,
      Vector(
        Execute(WorkflowJob(AgentPath("/AGENT"), ExecutablePath("/A")))),
      source = Some(source)))
  }

  private def check(source: String, workflow: Workflow): Unit = {
    assert(WorkflowParser.parse(source) == Valid(workflow.copy(source = Some(source))))
    val generatedSource = workflow.show
    assert(WorkflowParser.parse(generatedSource) == Valid(workflow.copy(source = Some(generatedSource))), "(generated source)")
  }

  private def parse(workflowString: String): Workflow =
    WorkflowParser.parse(workflowString) match {
      case Valid(workflow) ⇒ workflow
      case Invalid(problem) ⇒ throw new AssertionError(problem.toString, problem.throwableOption.orNull) with NoStackTrace
    }
}
