package com.sos.jobscheduler.data.workflow.parser

import cats.data.Validated.{Invalid, Valid}
import cats.syntax.show._
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.data.agent.AgentRefPath
import com.sos.jobscheduler.data.expression.Expression.{Equal, In, ListExpression, NumericConstant, Or, OrderReturnCode, StringConstant, Variable}
import com.sos.jobscheduler.data.job.{ExecutablePath, ExecutableScript, ReturnCode}
import com.sos.jobscheduler.data.order.OrderId
import com.sos.jobscheduler.data.source.SourcePos
import com.sos.jobscheduler.data.workflow.WorkflowPrinter.WorkflowShow
import com.sos.jobscheduler.data.workflow.instructions.executable.WorkflowJob
import com.sos.jobscheduler.data.workflow.instructions.{AwaitOrder, Execute, ExplicitEnd, Fail, Fork, Goto, If, IfNonZeroReturnCodeGoto, ImplicitEnd, Offer, Retry, ReturnCodeMeaning, TryInstruction}
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
    assert(parse(TestWorkflowSource).withoutSourcePos == TestWorkflow.withId(WorkflowPath.NoId))
  }

  "Unknown job" in {
    val source = """
      define workflow {
        if (true) {
          job A;
        }
      }"""
    assert(WorkflowParser.parse(source)
      == Invalid(Problem("""Expected known job name ('A' is unknown):6:8, found """"")))  // TODO Wrong position in error message, should be 4:12
  }

  "Execute anonymous" in {
    check("""define workflow { execute executable="/my/executable", agent="/AGENT"; }""",
      Workflow.of(
        Execute.Anonymous(
          WorkflowJob(AgentRefPath("/AGENT"), ExecutablePath("/my/executable")),
          sourcePos(18, 69)),
        ImplicitEnd(sourcePos(71, 72))))
  }

  "Execute anonymous with relative agent path" in {
    check("""define workflow { execute executable="/my/executable", agent="AGENT"; }""",
      Workflow.of(
        Execute.Anonymous(
          WorkflowJob(AgentRefPath("/AGENT"), ExecutablePath("/my/executable")),
          sourcePos(18, 68)),
        ImplicitEnd(sourcePos(70, 71))))
  }

  "Execute anonymous with default arguments 'SCHEDULER_PARAM_'" in {
    check("""define workflow { execute executable="/my/executable", agent="/AGENT", arguments={"A": "aaa", "B": "bbb"}, taskLimit=3; }""",
      Workflow.of(
        Execute.Anonymous(
          WorkflowJob(AgentRefPath("/AGENT"),
            ExecutablePath("/my/executable"),
            Map("A" -> "aaa", "B" -> "bbb"),
            taskLimit = 3),
          sourcePos(18, 118)),
        ImplicitEnd(sourcePos(120, 121))))
  }

  "Execute script with \\n" in {
    check(
      """define workflow { execute script="LINE 1\nLINE 2\nLINE 3", agent="/AGENT"; }""",
      Workflow.of(
        Execute.Anonymous(
          WorkflowJob(AgentRefPath("/AGENT"), ExecutableScript("LINE 1\nLINE 2\nLINE 3")),
          sourcePos(18, 73)),
        ImplicitEnd(sourcePos(75, 76))))
  }

  "Execute script with multi-line string" in {
    check(
"""define workflow {
  execute agent="/AGENT", script=
   'LINE 1
   |LINE 2
   |LINE 3
   |'.stripMargin;
}""",
      Workflow.of(
        Execute.Anonymous(
          WorkflowJob(AgentRefPath("/AGENT"), ExecutableScript("LINE 1\nLINE 2\nLINE 3\n")),
          sourcePos(20, 102)),
        ImplicitEnd(sourcePos(104, 105))))
  }

  "Execute named" in {
    check("""
      define workflow {
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
          execute script="SCRIPT", agent="/AGENT"
        }
      }""",
      Workflow(
        WorkflowPath.NoId,
        Vector(
          Execute.Named(WorkflowJob.Name("A"), sourcePos = sourcePos(33, 38)),
          Execute.Named(WorkflowJob.Name("B"), defaultArguments = Map("KEY" -> "VALUE"), sourcePos(48, 85)),
          Execute.Named(WorkflowJob.Name("C"), sourcePos = sourcePos(95, 100)),
          ImplicitEnd(sourcePos(412, 413))),
        Map(
          WorkflowJob.Name("A") ->
            WorkflowJob(
              AgentRefPath("/AGENT"),
              ExecutablePath("/my/executable"),
              returnCodeMeaning = ReturnCodeMeaning.Success.of(0, 1, 3)),
          WorkflowJob.Name("B") ->
            WorkflowJob(
              AgentRefPath("/AGENT"),
              ExecutablePath("/my/executable")),
          WorkflowJob.Name("C") ->
            WorkflowJob(
              AgentRefPath("/AGENT"),
              ExecutableScript("SCRIPT")))))
  }

  "Execute named with duplicate jobs" in {
    assert(WorkflowParser.parse("""
      define workflow {
        job DUPLICATE;
        define job DUPLICATE {
          execute executable="/my/executable", agent="/AGENT";
        }
        define job DUPLICATE {
          execute executable="/my/executable", agent="/AGENT"
        }
      }""")
      == Invalid(Problem("""Expected unique job definitions (duplicates: DUPLICATE):10:8, found """"")))
  }

  "Single instruction with relative job path" in {
    check("""define workflow { execute executable="/A", agent="AGENT"; }""",
      Workflow.anonymous(
        Vector(
          Execute.Anonymous(WorkflowJob(AgentRefPath("/AGENT"), ExecutablePath("/A")), sourcePos(18, 56)),
          ImplicitEnd(sourcePos(58, 59)))))
  }

  "Single instruction with absolute job path" in {
    check("""define workflow { execute executable="/A", agent="/AGENT"; }""",
      Workflow.anonymous(
        Vector(
          Execute.Anonymous(WorkflowJob(AgentRefPath("/AGENT"), ExecutablePath("/A")), sourcePos(18, 57)),
          ImplicitEnd(sourcePos(59, 60)))))
  }

  "execute with successReturnCodes" in {
    check("""define workflow { execute executable="/A", agent="AGENT", successReturnCodes=[0, 1, 3]; }""",
      Workflow.anonymous(
        Vector(
          Execute.Anonymous(
            WorkflowJob(AgentRefPath("/AGENT"), ExecutablePath("/A"), returnCodeMeaning = ReturnCodeMeaning.Success.of(0, 1, 3)),
            sourcePos(18, 86)),
          ImplicitEnd(sourcePos(88, 89)))))
  }

  "execute with failureReturnCodes" in {
    check("""define workflow { execute executable="/A", agent="AGENT", failureReturnCodes=[1, 3]; }""",
      Workflow.anonymous(
        Vector(
          Execute.Anonymous(
            WorkflowJob(AgentRefPath("/AGENT"), ExecutablePath("/A"), returnCodeMeaning = ReturnCodeMeaning.Failure.of(1, 3)),
            sourcePos(18, 83)),
          ImplicitEnd(sourcePos(85, 86)))))
  }

  "Label and single instruction" in {
    check("""define workflow { A: execute executable="/A", agent="AGENT"; }""",
      Workflow.anonymous(
        Vector(
          "A" @: Execute.Anonymous(WorkflowJob(AgentRefPath("/AGENT"), ExecutablePath("/A")), sourcePos(21, 59)),
          ImplicitEnd(sourcePos(61, 62)))))
  }

  "if (...) {...}" in {
    check("""define workflow { if ((returnCode in [1, 2]) || $KEY == "VALUE") { execute executable="/THEN", agent="AGENT" } }""",
      Workflow.anonymous(
        Vector(
          If(
            Or(
              In(OrderReturnCode, ListExpression(NumericConstant(1) :: NumericConstant(2) :: Nil)),
              Equal(Variable(StringConstant("KEY")), StringConstant("VALUE"))),
            Workflow.of(
              Execute.Anonymous(
                WorkflowJob(AgentRefPath("/AGENT"), ExecutablePath("/THEN")),
                sourcePos = sourcePos(67, 108)),
              ImplicitEnd(sourcePos(109, 110))),
            sourcePos = sourcePos(18, 64)),
          ImplicitEnd(sourcePos(111, 112)))))
  }

  "if (...) {...} else {...}" in {
    check("""define workflow { if (returnCode == -1) { execute executable="/THEN", agent="AGENT" } else { execute executable="/ELSE", agent="AGENT" } }""",
      Workflow.anonymous(
        Vector(
          If(Equal(OrderReturnCode, NumericConstant(-1)),
            Workflow.of(
              Execute.Anonymous(
                WorkflowJob(AgentRefPath("/AGENT"), ExecutablePath("/THEN")),
                sourcePos(42, 83)),
              ImplicitEnd(sourcePos(84, 85))),
            Some(Workflow.of(
              Execute.Anonymous(
                WorkflowJob(AgentRefPath("/AGENT"), ExecutablePath("/ELSE")),
                sourcePos(93, 134)),
              ImplicitEnd(sourcePos(135, 136)))),
            sourcePos(18, 39)),
          ImplicitEnd(sourcePos(137, 138)))))
  }

 "if (...) instruction" in {
    check("""define workflow { if (returnCode == -1) fail }""",
      Workflow.anonymous(
        Vector(
          If(Equal(OrderReturnCode, NumericConstant(-1)),
            Workflow.of(Fail(sourcePos = sourcePos(40, 44))),
            sourcePos = sourcePos(18, 39)),
          ImplicitEnd(sourcePos(45, 46)))))
  }

 "if (...) instruction else instruction" in {
    check("""define workflow { if (returnCode == -1) fail else execute executable="/ELSE", agent="AGENT" }""",
      Workflow.anonymous(
        Vector(
          If(Equal(OrderReturnCode, NumericConstant(-1)),
            Workflow.of(Fail(sourcePos = sourcePos(40, 44))),
            Some(Workflow.of(Execute.Anonymous(
              WorkflowJob(AgentRefPath("/AGENT"), ExecutablePath("/ELSE")),
              sourcePos = sourcePos(50, 91)))),
            sourcePos(18, 39)),
          ImplicitEnd(sourcePos(92, 93)))))
  }

  "Two consecutive ifs with semicolon" in {
    check(
     """define workflow {
          if (returnCode == 1) {}
          if (returnCode == 2) {}
        }""",
      Workflow.anonymous(
        Vector(
          If(
            Equal(OrderReturnCode, NumericConstant(1)),
            Workflow.of(
              ImplicitEnd(sourcePos(50, 51))),
            sourcePos = sourcePos(28, 48)),
          If(Equal(OrderReturnCode, NumericConstant(2)),
            Workflow.of(
              ImplicitEnd(sourcePos(84, 85))),
            sourcePos = sourcePos(62, 82)),
          ImplicitEnd(sourcePos(94, 95)))))
  }

  "Two consecutive ifs without semicolon" in {
    check(
     """define workflow {
          if (returnCode == 1) {
          }
          if (returnCode == 2) {
          }
        }""",
      Workflow.anonymous(
        Vector(
          If(
            Equal(OrderReturnCode, NumericConstant(1)),
            Workflow.of(
              ImplicitEnd(sourcePos(61, 62))),
            sourcePos = sourcePos(28, 48)),
          If(Equal(OrderReturnCode, NumericConstant(2)),
            Workflow.of(
              ImplicitEnd(sourcePos(106, 107))),
            sourcePos = sourcePos(73, 93)),
          ImplicitEnd(sourcePos(116, 117)))))
  }

  "fork" in {
    check(
      """define workflow {
           fork {
             "🥕": {
               execute executable="/a", agent="/agent-a";
             },
             "🍋": execute executable="/b", agent="/agent-b";
           }
         }""",
      Workflow.of(
        Fork(
          Vector(
            Fork.Branch("🥕", Workflow.of(
              Execute.Anonymous(
                WorkflowJob(AgentRefPath("/agent-a"), ExecutablePath("/a")),
                sourcePos(71+1, 112+1)),
              ImplicitEnd(sourcePos(127+1, 128+1)))),
            Fork.Branch("🍋", Workflow.of(
              Execute.Anonymous(
                WorkflowJob(AgentRefPath("/agent-b"), ExecutablePath("/b")),
                sourcePos(148+2, 189+2))))),
            sourcePos(29, 33)),
        ImplicitEnd(sourcePos(213+2, 214+2))))
  }

  "offer" in {
    check("""define workflow { offer orderId = "OFFERED", timeout = 60; }""",
      Workflow(WorkflowPath.NoId, Vector(
        Offer(OrderId("OFFERED"), 60.seconds, sourcePos(18, 57)),
        ImplicitEnd(sourcePos(59, 60)))))
  }

  "await" in {
    check("""define workflow { await orderId = "OFFERED"; }""",
      Workflow(WorkflowPath.NoId, Vector(
        AwaitOrder(OrderId("OFFERED"), sourcePos(18, 43)),
        ImplicitEnd(sourcePos(45, 46)))))
  }

  "try" - {
    "try" in {
      check("""
        define workflow {
          try {
            execute executable="/TRY", agent="/AGENT";
          } catch {
            execute executable="/CATCH", agent="/AGENT";
          }
        }""",
        Workflow(WorkflowPath.NoId, Vector(
          TryInstruction(
            Workflow.of(
              Execute.Anonymous(
                WorkflowJob(AgentRefPath("/AGENT"), ExecutablePath("/TRY")),
                sourcePos(55, 96)),
              ImplicitEnd(sourcePos(108, 109))),
            Workflow.of(
              Execute.Anonymous(
                WorkflowJob(AgentRefPath("/AGENT"), ExecutablePath("/CATCH")),
                sourcePos(130, 173)),
              ImplicitEnd(sourcePos(185, 186))),
            sourcePos = sourcePos(37, 40)),
          ImplicitEnd(sourcePos(195, 196))))
      )
    }

    "try with retryDelays" in {
      check("""
        define workflow {
          try (retryDelays=[1, 2, 3]) fail;
          catch retry;
        }""",
        Workflow(WorkflowPath.NoId, Vector(
          TryInstruction(
            Workflow.of(
              Fail(sourcePos = sourcePos(65, 69))),
            Workflow.of(
              Retry(sourcePos(87, 92))),
            Some(Vector(1.second, 2.seconds, 3.seconds)),
            sourcePos(37, 64)),
          ImplicitEnd(sourcePos(102, 103)))))
    }

    "try with retryDelays but retry is missing" in {
      assert(WorkflowParser.parse("""
        define workflow {
          try (retryDelays=[1, 2, 3]) fail;
          catch {}
        }""") ==
        Invalid(Problem("""Expected Missing a retry instruction in the catch block to make sense of retryDelays:5:9, found "}"""")))
    }
  }

  "retry" - {
    "no delay" in {
      check("""
        define workflow {
          try {
            fail;
          } catch {
            retry;
          }
        }""",
        Workflow.of(
          TryInstruction(
            Workflow.of(
              Fail(sourcePos = sourcePos(55, 59)),
              ImplicitEnd(sourcePos(71, 72))),
            Workflow.of(
              Retry(sourcePos(93, 98)),
              ImplicitEnd(sourcePos(110, 111))),
            sourcePos = sourcePos(37, 40)),
          ImplicitEnd(sourcePos(120, 121))))
    }
  }

  "fail" in {
    check("""
      define workflow {
        fail returnCode=7;
        fail;
      }""",
      Workflow(WorkflowPath.NoId, Vector(
        Fail(Some(ReturnCode(7)), sourcePos(33, 50)),
        Fail(None, sourcePos(60, 64)),
        ImplicitEnd(sourcePos(72, 73)))))
  }

  "onError and goto" in {
    check("""
      define workflow {
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
        Execute.Anonymous(
          WorkflowJob(AgentRefPath("/AGENT"), ExecutablePath("/A")),
          sourcePos(33, 72)),
        IfNonZeroReturnCodeGoto(Label("FAILURE"), sourcePos(82, 113)),
        Execute.Anonymous(
          WorkflowJob(AgentRefPath("/AGENT"), ExecutablePath("/B")),
          sourcePos(123, 162)),
        Goto(Label("END"), sourcePos(172, 180)),
        "FAILURE" @:
        Execute.Anonymous(
          WorkflowJob(AgentRefPath("/AGENT"), ExecutablePath("/OnFailure")),
          sourcePos(199, 246)),
        "END" @:
        ExplicitEnd(sourcePos(261, 264)))))
  }

  //for (n <- sys.props.get("test.speed") map (_.toInt)) "Speed" - {
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
        define workflow {
          //comment
          /*comment/**/execute/***/executable="/A"/**/,agent/**/=/**/"AGENT"/**/;/**///comment
        }
      """
    assert(parse(source) == Workflow(
      WorkflowPath.NoId,
      Vector(
        Execute.Anonymous(
          WorkflowJob(AgentRefPath("/AGENT"), ExecutablePath("/A")),
          sourcePos(90, 143)),
        ImplicitEnd(sourcePos(170, 171))),
      source = Some(source)))
  }

  private def sourcePos(start: Int, end: Int) = Some(SourcePos(start, end))

  private def check(source: String, workflow: Workflow): Unit = {
    assert(WorkflowParser.parse(source) == Valid(workflow.copy(source = Some(source))))
    val generatedSource = workflow.show
    assert(WorkflowParser.parse(generatedSource).map(_.withoutSourcePos)
      == Valid(workflow.copy(source = Some(generatedSource)).withoutSourcePos),
      s"(generated source: $generatedSource)")
  }

  private def parse(workflowString: String): Workflow =
    WorkflowParser.parse(workflowString) match {
      case Valid(workflow) => workflow
      case Invalid(problem) => throw new AssertionError(problem.toString, problem.throwableOption.orNull) with NoStackTrace
    }
}
