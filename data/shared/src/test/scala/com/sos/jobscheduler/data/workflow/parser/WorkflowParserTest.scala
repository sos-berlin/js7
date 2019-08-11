package com.sos.jobscheduler.data.workflow.parser

import cats.syntax.show._
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.data.agent.AgentRefPath
import com.sos.jobscheduler.data.expression.Expression.{Equal, In, LastReturnCode, ListExpression, NamedValue, NumericConstant, Or, StringConstant}
import com.sos.jobscheduler.data.job.{ExecutablePath, ExecutableScript, ReturnCode}
import com.sos.jobscheduler.data.order.OrderId
import com.sos.jobscheduler.data.source.SourcePos
import com.sos.jobscheduler.data.workflow.WorkflowPrinter.WorkflowShow
import com.sos.jobscheduler.data.workflow.instructions.executable.WorkflowJob
import com.sos.jobscheduler.data.workflow.instructions.{AwaitOrder, Execute, ExplicitEnd, Fail, Finish, Fork, Goto, If, IfFailedGoto, ImplicitEnd, Offer, Retry, ReturnCodeMeaning, TryInstruction}
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
      == Left(Problem("""Expected known job name ('A' is unknown):6:8, found """"")))  // TODO Wrong position in error message, should be 4:12
  }

  "Execute anonymous" in {
    checkWithSourcePos("""define workflow { execute executable="/my/executable", agent="/AGENT"; }""",
      Workflow.of(
        Execute.Anonymous(
          WorkflowJob(AgentRefPath("/AGENT"), ExecutablePath("/my/executable")),
          sourcePos(18, 69)),
        ImplicitEnd(sourcePos(71, 72))))
  }

  "Execute anonymous with relative agent path" in {
    checkWithSourcePos("""define workflow { execute executable="/my/executable", agent="AGENT"; }""",
      Workflow.of(
        Execute.Anonymous(
          WorkflowJob(AgentRefPath("/AGENT"), ExecutablePath("/my/executable")),
          sourcePos(18, 68)),
        ImplicitEnd(sourcePos(70, 71))))
  }

  "Execute anonymous with default arguments 'SCHEDULER_PARAM_'" in {
    checkWithSourcePos("""define workflow { execute executable="/my/executable", agent="/AGENT", arguments={"A": "aaa", "B": "bbb"}, taskLimit=3; }""",
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
    checkWithSourcePos(
      """define workflow { execute script="LINE 1\nLINE 2\nLINE 3", agent="/AGENT"; }""",
      Workflow.of(
        Execute.Anonymous(
          WorkflowJob(AgentRefPath("/AGENT"), ExecutableScript("LINE 1\nLINE 2\nLINE 3")),
          sourcePos(18, 73)),
        ImplicitEnd(sourcePos(75, 76))))
  }

  "Execute script with multi-line string" in {
    checkWithSourcePos(
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
    checkWithSourcePos("""
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
      == Left(Problem("""Expected unique job definitions (duplicates: DUPLICATE):10:8, found """"")))
  }

  "Single instruction with relative job path" in {
    checkWithSourcePos("""define workflow { execute executable="/A", agent="AGENT"; }""",
      Workflow.anonymous(
        Vector(
          Execute.Anonymous(WorkflowJob(AgentRefPath("/AGENT"), ExecutablePath("/A")), sourcePos(18, 56)),
          ImplicitEnd(sourcePos(58, 59)))))
  }

  "Single instruction with absolute job path" in {
    checkWithSourcePos("""define workflow { execute executable="/A", agent="/AGENT"; }""",
      Workflow.anonymous(
        Vector(
          Execute.Anonymous(WorkflowJob(AgentRefPath("/AGENT"), ExecutablePath("/A")), sourcePos(18, 57)),
          ImplicitEnd(sourcePos(59, 60)))))
  }

  "execute with successReturnCodes" in {
    checkWithSourcePos("""define workflow { execute executable="/A", agent="AGENT", successReturnCodes=[0, 1, 3]; }""",
      Workflow.anonymous(
        Vector(
          Execute.Anonymous(
            WorkflowJob(AgentRefPath("/AGENT"), ExecutablePath("/A"), returnCodeMeaning = ReturnCodeMeaning.Success.of(0, 1, 3)),
            sourcePos(18, 86)),
          ImplicitEnd(sourcePos(88, 89)))))
  }

  "execute with failureReturnCodes" in {
    checkWithSourcePos("""define workflow { execute executable="/A", agent="AGENT", failureReturnCodes=[1, 3]; }""",
      Workflow.anonymous(
        Vector(
          Execute.Anonymous(
            WorkflowJob(AgentRefPath("/AGENT"), ExecutablePath("/A"), returnCodeMeaning = ReturnCodeMeaning.Failure.of(1, 3)),
            sourcePos(18, 83)),
          ImplicitEnd(sourcePos(85, 86)))))
  }

  "Label and single instruction" in {
    checkWithSourcePos("""define workflow { A: execute executable="/A", agent="AGENT"; }""",
      Workflow.anonymous(
        Vector(
          "A" @: Execute.Anonymous(WorkflowJob(AgentRefPath("/AGENT"), ExecutablePath("/A")), sourcePos(21, 59)),
          ImplicitEnd(sourcePos(61, 62)))))
  }

  "if (...) {...}" in {
    checkWithSourcePos("""define workflow { if ((returnCode in [1, 2]) || $KEY == "VALUE") { execute executable="/THEN", agent="AGENT" } }""",
      Workflow.anonymous(
        Vector(
          If(
            Or(
              In(LastReturnCode, ListExpression(NumericConstant(1) :: NumericConstant(2) :: Nil)),
              Equal(NamedValue.last("KEY"), StringConstant("VALUE"))),
            Workflow.of(
              Execute.Anonymous(
                WorkflowJob(AgentRefPath("/AGENT"), ExecutablePath("/THEN")),
                sourcePos = sourcePos(67, 108)),
              ImplicitEnd(sourcePos(109, 110))),
            sourcePos = sourcePos(18, 64)),
          ImplicitEnd(sourcePos(111, 112)))))
  }

  "if (...) {...} else {...}" in {
    checkWithSourcePos("""define workflow { if (returnCode == -1) { execute executable="/THEN", agent="AGENT" } else { execute executable="/ELSE", agent="AGENT" } }""",
      Workflow.anonymous(
        Vector(
          If(Equal(LastReturnCode, NumericConstant(-1)),
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
    checkWithSourcePos("""define workflow { if (returnCode == -1) fail }""",
      Workflow.anonymous(
        Vector(
          If(Equal(LastReturnCode, NumericConstant(-1)),
            Workflow.of(Fail(sourcePos = sourcePos(40, 44))),
            sourcePos = sourcePos(18, 39)),
          ImplicitEnd(sourcePos(45, 46)))))
  }

 "if (...) instruction else instruction" in {
    checkWithSourcePos("""define workflow { if (returnCode == -1) fail else execute executable="/ELSE", agent="AGENT" }""",
      Workflow.anonymous(
        Vector(
          If(Equal(LastReturnCode, NumericConstant(-1)),
            Workflow.of(Fail(sourcePos = sourcePos(40, 44))),
            Some(Workflow.of(Execute.Anonymous(
              WorkflowJob(AgentRefPath("/AGENT"), ExecutablePath("/ELSE")),
              sourcePos = sourcePos(50, 91)))),
            sourcePos(18, 39)),
          ImplicitEnd(sourcePos(92, 93)))))
  }

  "Two consecutive ifs with semicolon" in {
    checkWithSourcePos(
     """define workflow {
          if (returnCode == 1) {}
          if (returnCode == 2) {}
        }""",
      Workflow.anonymous(
        Vector(
          If(
            Equal(LastReturnCode, NumericConstant(1)),
            Workflow.of(
              ImplicitEnd(sourcePos(50, 51))),
            sourcePos = sourcePos(28, 48)),
          If(Equal(LastReturnCode, NumericConstant(2)),
            Workflow.of(
              ImplicitEnd(sourcePos(84, 85))),
            sourcePos = sourcePos(62, 82)),
          ImplicitEnd(sourcePos(94, 95)))))
  }

  "Two consecutive ifs without semicolon" in {
    checkWithSourcePos(
     """define workflow {
          if (returnCode == 1) {
          }
          if (returnCode == 2) {
          }
        }""",
      Workflow.anonymous(
        Vector(
          If(
            Equal(LastReturnCode, NumericConstant(1)),
            Workflow.of(
              ImplicitEnd(sourcePos(61, 62))),
            sourcePos = sourcePos(28, 48)),
          If(Equal(LastReturnCode, NumericConstant(2)),
            Workflow.of(
              ImplicitEnd(sourcePos(106, 107))),
            sourcePos = sourcePos(73, 93)),
          ImplicitEnd(sourcePos(116, 117)))))
  }

  "fork" in {
    checkWithSourcePos(
      """define workflow {
           fork {
             "🥕": {
               execute executable="/a", agent="/agent-a";
             },
             "🍋": execute executable="/b", agent="/agent-b";
           }
         }""",
      Workflow.of(
        Fork.forTest(Vector(
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
    checkWithSourcePos("""define workflow { offer orderId = "OFFERED", timeout = 60; }""",
      Workflow(WorkflowPath.NoId, Vector(
        Offer(OrderId("OFFERED"), 60.seconds, sourcePos(18, 57)),
        ImplicitEnd(sourcePos(59, 60)))))
  }

  "await" in {
    checkWithSourcePos("""define workflow { await orderId = "OFFERED"; }""",
      Workflow(WorkflowPath.NoId, Vector(
        AwaitOrder(OrderId("OFFERED"), sourcePos(18, 43)),
        ImplicitEnd(sourcePos(45, 46)))))
  }

  "try" - {
    "try" in {
      checkWithSourcePos("""
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
      checkWithSourcePos("""
        define workflow {
          try (retryDelays=[1, 2, 3], maxTries=3) fail;
          catch retry;
        }""",
        Workflow(WorkflowPath.NoId, Vector(
          TryInstruction(
            Workflow.of(
              Fail(sourcePos = sourcePos(77, 81))),
            Workflow.of(
              Retry(sourcePos(99, 104))),
            Some(Vector(1.second, 2.seconds, 3.seconds)),
            maxTries = Some(3),
            sourcePos = sourcePos(37, 76)),
          ImplicitEnd(sourcePos(114, 115)))))
    }

    "try with retryDelays but retry is missing" in {
      assert(WorkflowParser.parse("""
        define workflow {
          try (retryDelays=[1, 2, 3]) fail;
          catch {}
        }""") ==
        Left(Problem("""Expected Missing a retry instruction in the catch block to make sense of retryDelays or maxTries:5:9, found "}"""")))
    }

    "try with maxRetries but retry is missing" in {
      assert(WorkflowParser.parse("""
        define workflow {
          try (maxTries=3) fail;
          catch {}
        }""") ==
        Left(Problem("""Expected Missing a retry instruction in the catch block to make sense of retryDelays or maxTries:5:9, found "}"""")))
    }
  }

  "retry" - {
    "no delay" in {
      checkWithSourcePos("""
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
    checkWithSourcePos("""
      define workflow {
        fail;
        fail (returnCode=7);
        fail (message="ERROR");
        fail (message="ERROR", returnCode=7);
        fail (uncatchable=true, message="ERROR", returnCode=7);
      }""",
      Workflow(WorkflowPath.NoId, Vector(
        Fail(None, None, sourcePos = sourcePos(33, 37)),
        Fail(None, Some(ReturnCode(7)), sourcePos = sourcePos(47, 66)),
        Fail(Some(StringConstant("ERROR")), None, sourcePos = sourcePos(76, 98)),
        Fail(Some(StringConstant("ERROR")), Some(ReturnCode(7)), sourcePos = sourcePos(108, 144)),
        Fail(Some(StringConstant("ERROR")), Some(ReturnCode(7)), uncatchable = true, sourcePos(154, 208)),
        ImplicitEnd(sourcePos = sourcePos(216, 217)))))
  }

  "finish" in {
    checkWithSourcePos("""
      define workflow {
        finish;
      }""",
      Workflow(WorkflowPath.NoId, Vector(
        Finish(sourcePos(33, 39)),
        ImplicitEnd(sourcePos = sourcePos(47, 48)))))
  }

  "onError and goto" in {
    checkWithSourcePos("""
      define workflow {
        execute executable="/A", agent="/AGENT";
        ifFailedGoto FAILURE;
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
        IfFailedGoto(Label("FAILURE"), sourcePos(82, 102)),
        Execute.Anonymous(
          WorkflowJob(AgentRefPath("/AGENT"), ExecutablePath("/B")),
          sourcePos(112, 151)),
        Goto(Label("END"), sourcePos(161, 169)),
        "FAILURE" @:
        Execute.Anonymous(
          WorkflowJob(AgentRefPath("/AGENT"), ExecutablePath("/OnFailure")),
          sourcePos(188, 235)),
        "END" @:
        ExplicitEnd(sourcePos(250, 253)))))
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

  private def checkWithSourcePos(source: String, workflow: Workflow): Unit =
    check2(source, workflow, withSourcePos = true)

  private def check(source: String, workflow: Workflow): Unit =
    check2(source, workflow, withSourcePos = false)

  private def check2(source: String, workflow: Workflow, withSourcePos: Boolean): Unit = {
    val parsedWorkflow = WorkflowParser.parse(source).map(o => if (withSourcePos) o else o.withoutSourcePos)
    assert(parsedWorkflow == Right(workflow.copy(source = Some(source))))
    val generatedSource = workflow.show
    assert(WorkflowParser.parse(generatedSource).map(_.withoutSourcePos)
      == Right(workflow.copy(source = Some(generatedSource)).withoutSourcePos),
      s"(generated source: $generatedSource)")
  }

  private def parse(workflowString: String): Workflow =
    WorkflowParser.parse(workflowString) match {
      case Right(workflow) => workflow
      case Left(problem) => throw new AssertionError(problem.toString, problem.throwableOption.orNull) with NoStackTrace
    }
}
