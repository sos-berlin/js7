package js7.data.workflow

import cats.syntax.show._
import com.softwaremill.diffx.generic.auto._
import js7.base.problem.Checked._
import js7.base.problem.Problem
import js7.base.time.ScalaTime._
import js7.data.agent.AgentPath
import js7.data.job.{CommandLineExecutable, PathExecutable, ScriptExecutable}
import js7.data.lock.LockPath
import js7.data.order.OrderId
import js7.data.source.SourcePos
import js7.data.value.expression.Expression.{Equal, In, LastReturnCode, ListExpression, NamedValue, NumericConstant, ObjectExpression, Or, StringConstant}
import js7.data.value.{NumberValue, StringValue}
import js7.data.workflow.WorkflowPrinter.WorkflowShow
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.instructions.{AwaitOrder, Execute, ExplicitEnd, Fail, Finish, Fork, Goto, If, IfFailedGoto, ImplicitEnd, LockInstruction, Offer, Retry, ReturnCodeMeaning, TryInstruction}
import js7.data.workflow.test.ForkTestSetting.{TestWorkflow, TestWorkflowSource}
import js7.tester.DiffxAssertions.assertEqual
import org.scalatest.freespec.AnyFreeSpec
import scala.util.control.NoStackTrace

/**
  * @author Joacim Zschimmer
  */
final class WorkflowParserTest extends AnyFreeSpec
{
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
      == Left(Problem("""Expected Unknown job name 'A':6:8, found """"")))  // TODO Wrong position in error message, should be 4:12
  }

  "Execute anonymous" in {
    checkWithSourcePos("""define workflow { execute executable="my/executable", agent="AGENT"; }""",
      Workflow.of(
        Execute.Anonymous(
          WorkflowJob(AgentPath("AGENT"), PathExecutable("my/executable")),
          sourcePos = sourcePos(18, 67)),
        ImplicitEnd(sourcePos(69, 70))))
  }

  "Execute anonymous with relative agent path" in {
    checkWithSourcePos("""define workflow { execute executable="my/executable", agent="AGENT"; }""",
      Workflow.of(
        Execute.Anonymous(
          WorkflowJob(AgentPath("AGENT"), PathExecutable("my/executable")),
          sourcePos= sourcePos(18, 67)),
        ImplicitEnd(sourcePos(69, 70))))
  }

  "Execute anonymous with default arguments 'SCHEDULER_PARAM_'" in {
    checkWithSourcePos(
       """define workflow {
         |  execute executable = "my/executable",
         |          agent = "AGENT",
         |          defaultArguments = { "A": "aaa", "B": "bbb", "I": -123 },
         |          taskLimit = 3,
         |          sigkillDelay = 30;
         |}""".stripMargin,
      Workflow.of(
        Execute.Anonymous(
          WorkflowJob(AgentPath("AGENT"),
            PathExecutable("my/executable"),
            Map(
              "A" -> StringValue("aaa"),
              "B" -> StringValue("bbb"),
              "I" -> NumberValue(-123)),
            taskLimit = 3,
            sigkillDelay = Some(30.s)),
          sourcePos = sourcePos(20, 205)),
        ImplicitEnd(sourcePos(207, 208))))
  }

  "Execute script with \\n" in {
    checkWithSourcePos(
      """define workflow { execute script="LINE 1\nLINE 2\nLINE 3", agent="AGENT"; }""",
      Workflow.of(
        Execute.Anonymous(
          WorkflowJob(AgentPath("AGENT"), ScriptExecutable("LINE 1\nLINE 2\nLINE 3")),
          sourcePos = sourcePos(18, 72)),
        ImplicitEnd(sourcePos(74, 75))))
  }

  "Execute script with multi-line string" in {
    checkWithSourcePos(
"""define workflow {
  execute agent="AGENT", script=
   'LINE 1
   |LINE 2
   |LINE 3
   |'.stripMargin;
}""",
      Workflow.of(
        Execute.Anonymous(
          WorkflowJob(AgentPath("AGENT"), ScriptExecutable("LINE 1\nLINE 2\nLINE 3\n")),
          sourcePos = sourcePos(20, 101)),
        ImplicitEnd(sourcePos(103, 104))))
  }

  "Execute named" in {
    checkWithSourcePos("""
      define workflow {
        job A;
        job B, defaultArguments = { "KEY": "VALUE" };
        job C;
        define job A {
          execute executable="my/executable", agent="AGENT", successReturnCodes=[0, 1, 3];
        }
        define job B {
          execute executable="my/executable", agent="AGENT"
        }
        define job C {
          execute script="SCRIPT", agent="AGENT"
        }
      }""",
      Workflow(
        WorkflowPath.NoId,
        Vector(
          Execute.Named(WorkflowJob.Name("A"), sourcePos = sourcePos(33, 38)),
          Execute.Named(WorkflowJob.Name("B"), defaultArguments = Map("KEY" -> StringValue("VALUE")), sourcePos(48, 92)),
          Execute.Named(WorkflowJob.Name("C"), sourcePos = sourcePos(102, 107)),
          ImplicitEnd(sourcePos(414, 415))),
        Map(
          WorkflowJob.Name("A") ->
            WorkflowJob(
              AgentPath("AGENT"),
              PathExecutable("my/executable"),
              returnCodeMeaning = ReturnCodeMeaning.Success.of(0, 1, 3)),
          WorkflowJob.Name("B") ->
            WorkflowJob(
              AgentPath("AGENT"),
              PathExecutable("my/executable")),
          WorkflowJob.Name("C") ->
            WorkflowJob(
              AgentPath("AGENT"),
              ScriptExecutable("SCRIPT")))))
  }

  "define job" in {
    check("""
      define workflow {
        job A;
        define job A {
          execute
            agent="AGENT",
            executable="my/executable",
            successReturnCodes=[0, 1, 3],
            env={
              "A": 1,
              "B": $b
            };
        }
      }""",
      Workflow(
        WorkflowPath.NoId,
        Vector(
          Execute.Named(WorkflowJob.Name("A"))),
        Map(
          WorkflowJob.Name("A") ->
            WorkflowJob(
              AgentPath("AGENT"),
              PathExecutable(
                "my/executable",
                env = ObjectExpression(Map(
                  "A" -> NumericConstant(1),
                  "B" -> NamedValue.last("b")))),
              returnCodeMeaning = ReturnCodeMeaning.Success.of(0, 1, 3)))))
  }

  "Execute named with duplicate jobs" in {
    assert(WorkflowParser.parse("""
      define workflow {
        job DUPLICATE;
        define job DUPLICATE {
          execute executable="my/executable", agent="AGENT";
        }
        define job DUPLICATE {
          execute executable="my/executable", agent="AGENT"
        }
      }""")
      == Left(Problem("""Expected unique job definitions (duplicates: DUPLICATE):10:8, found """"")))
  }

  "Single instruction with relative job path" in {
    checkWithSourcePos("""define workflow { execute executable="A", agent="AGENT"; }""",
      Workflow.anonymous(
        Vector(
          Execute.Anonymous(WorkflowJob(AgentPath("AGENT"), PathExecutable("A")), sourcePos = sourcePos(18, 55)),
          ImplicitEnd(sourcePos(57, 58)))))
  }

  "Single instruction with absolute job path" in {
    checkWithSourcePos("""define workflow { execute executable="A", agent="AGENT"; }""",
      Workflow.anonymous(
        Vector(
          Execute.Anonymous(WorkflowJob(AgentPath("AGENT"), PathExecutable("A")), sourcePos = sourcePos(18, 55)),
          ImplicitEnd(sourcePos(57, 58)))))
  }

  "execute with successReturnCodes" in {
    checkWithSourcePos("""define workflow { execute executable="A", agent="AGENT", successReturnCodes=[0, 1, 3]; }""",
      Workflow.anonymous(
        Vector(
          Execute.Anonymous(
            WorkflowJob(AgentPath("AGENT"), PathExecutable("A"), returnCodeMeaning = ReturnCodeMeaning.Success.of(0, 1, 3)),
            sourcePos = sourcePos(18, 85)),
          ImplicitEnd(sourcePos(87, 88)))))
  }

  "execute with failureReturnCodes" in {
    checkWithSourcePos("""define workflow { execute executable="A", agent="AGENT", failureReturnCodes=[1, 3]; }""",
      Workflow.anonymous(
        Vector(
          Execute.Anonymous(
            WorkflowJob(AgentPath("AGENT"), PathExecutable("A"), returnCodeMeaning = ReturnCodeMeaning.Failure.of(1, 3)),
            sourcePos = sourcePos(18, 82)),
          ImplicitEnd(sourcePos(84, 85)))))
  }

  "execute with command line" in {
    check("""define workflow { execute agent="AGENT", command="COMMAND"; }""",
      Workflow.anonymous(
        Vector(
          Execute.Anonymous(
            WorkflowJob(AgentPath("AGENT"), CommandLineExecutable.fromString("COMMAND").orThrow)))))
  }

  "Label and single instruction" in {
    checkWithSourcePos("""define workflow { A: execute executable="A", agent="AGENT"; }""",
      Workflow.anonymous(
        Vector(
          "A" @: Execute.Anonymous(WorkflowJob(AgentPath("AGENT"), PathExecutable("A")), sourcePos = sourcePos(21, 58)),
          ImplicitEnd(sourcePos(60, 61)))))
  }

  "if (...) {...}" in {
    checkWithSourcePos("""define workflow { if (($returnCode in [1, 2]) || $KEY == "VALUE") { execute executable="/THEN", agent="AGENT" } }""",
      Workflow.anonymous(
        Vector(
          If(
            Or(
              In(LastReturnCode, ListExpression(NumericConstant(1) :: NumericConstant(2) :: Nil)),
              Equal(NamedValue.last("KEY"), StringConstant("VALUE"))),
            Workflow.of(
              Execute.Anonymous(
                WorkflowJob(AgentPath("AGENT"), PathExecutable("/THEN")),
                sourcePos = sourcePos(68, 109)),
              ImplicitEnd(sourcePos(110, 111))),
            sourcePos = sourcePos(18, 65)),
          ImplicitEnd(sourcePos(112, 113)))))
  }

  "if (...) {...} else {...}" in {
    checkWithSourcePos("""define workflow { if ($returnCode == -1) { execute executable="/THEN", agent="AGENT" } else { execute executable="/ELSE", agent="AGENT" } }""",
      Workflow.anonymous(
        Vector(
          If(Equal(LastReturnCode, NumericConstant(-1)),
            Workflow.of(
              Execute.Anonymous(
                WorkflowJob(AgentPath("AGENT"), PathExecutable("/THEN")),
                sourcePos = sourcePos(43, 84)),
              ImplicitEnd(sourcePos(85, 86))),
            Some(Workflow.of(
              Execute.Anonymous(
                WorkflowJob(AgentPath("AGENT"), PathExecutable("/ELSE")),
                sourcePos = sourcePos(94, 135)),
              ImplicitEnd(sourcePos(136, 137)))),
            sourcePos = sourcePos(18, 40)),
          ImplicitEnd(sourcePos(138, 139)))))
  }

 "if (...) instruction" in {
    checkWithSourcePos("""define workflow { if ($returnCode == -1) fail }""",
      Workflow.anonymous(
        Vector(
          If(Equal(LastReturnCode, NumericConstant(-1)),
            Workflow.of(Fail(sourcePos = sourcePos(41, 45))),
            sourcePos = sourcePos(18, 40)),
          ImplicitEnd(sourcePos(46, 47)))))
  }

 "if (...) instruction else instruction" in {
    checkWithSourcePos("""define workflow { if ($returnCode == -1) fail else execute executable="/ELSE", agent="AGENT" }""",
      Workflow.anonymous(
        Vector(
          If(Equal(LastReturnCode, NumericConstant(-1)),
            Workflow.of(Fail(sourcePos = sourcePos(41, 45))),
            Some(Workflow.of(Execute.Anonymous(
              WorkflowJob(AgentPath("AGENT"), PathExecutable("/ELSE")),
              sourcePos = sourcePos(51, 92)))),
            sourcePos(18, 40)),
          ImplicitEnd(sourcePos(93, 94)))))
  }

  "Two consecutive ifs with semicolon" in {
    checkWithSourcePos(
     """define workflow {
          if ($returnCode == 1) {}
          if ($returnCode == 2) {}
        }""",
      Workflow.anonymous(
        Vector(
          If(
            Equal(LastReturnCode, NumericConstant(1)),
            Workflow.of(
              ImplicitEnd(sourcePos(51, 52))),
            sourcePos = sourcePos(28, 49)),
          If(Equal(LastReturnCode, NumericConstant(2)),
            Workflow.of(
              ImplicitEnd(sourcePos(86, 87))),
            sourcePos = sourcePos(63, 84)),
          ImplicitEnd(sourcePos(96, 97)))))
  }

  "Two consecutive ifs without semicolon" in {
    checkWithSourcePos(
     """define workflow {
          if ($returnCode == 1) {
          }
          if ($returnCode == 2) {
          }
        }""",
      Workflow.anonymous(
        Vector(
          If(
            Equal(LastReturnCode, NumericConstant(1)),
            Workflow.of(
              ImplicitEnd(sourcePos(62, 63))),
            sourcePos = sourcePos(28, 49)),
          If(Equal(LastReturnCode, NumericConstant(2)),
            Workflow.of(
              ImplicitEnd(sourcePos(108, 109))),
            sourcePos = sourcePos(74, 95)),
          ImplicitEnd(sourcePos(118, 119)))))
  }

  "fork" in {
    checkWithSourcePos(
      """define workflow {
           fork {
             "🥕": {
               execute executable="/a", agent="agent-a";
             },
             "🍋": execute executable="/b", agent="agent-b";
           }
         }""",
      Workflow.of(
        Fork.forTest(Vector(
            Fork.Branch("🥕", Workflow.of(
              Execute.Anonymous(
                WorkflowJob(AgentPath("agent-a"), PathExecutable("/a")),
                sourcePos = sourcePos(71+1, 111+1)),
              ImplicitEnd(sourcePos(126+1, 127+1)))),
            Fork.Branch("🍋", Workflow.of(
              Execute.Anonymous(
                WorkflowJob(AgentPath("agent-b"), PathExecutable("/b")),
                sourcePos = sourcePos(147+2, 187+2))))),
            sourcePos(29, 33)),
        ImplicitEnd(sourcePos(211+2, 212+2))))
  }

  "offer" in {
    checkWithSourcePos("""define workflow { offer orderId = "OFFERED", timeout = 60; }""",
      Workflow(WorkflowPath.NoId, Vector(
        Offer(OrderId("OFFERED"), 60.s, sourcePos(18, 57)),
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
            execute executable="/TRY", agent="AGENT";
          } catch {
            execute executable="/CATCH", agent="AGENT";
          }
        }""",
        Workflow(WorkflowPath.NoId, Vector(
          TryInstruction(
            Workflow.of(
              Execute.Anonymous(
                WorkflowJob(AgentPath("AGENT"), PathExecutable("/TRY")),
                sourcePos = sourcePos(55, 95)),
              ImplicitEnd(sourcePos(107, 108))),
            Workflow.of(
              Execute.Anonymous(
                WorkflowJob(AgentPath("AGENT"), PathExecutable("/CATCH")),
                sourcePos = sourcePos(129, 171)),
              ImplicitEnd(sourcePos(183, 184))),
            sourcePos = sourcePos(37, 40)),
          ImplicitEnd(sourcePos(193, 194))))
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
            Some(Vector(1.s, 2.s, 3.s)),
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
        fail (namedValues = { "returnCode": 7 });
        fail (message="ERROR");
        fail (message="ERROR", namedValues = { "returnCode": 7 });
        fail (uncatchable=true, message="ERROR", namedValues = { "returnCode": 7 });
      }""",
      Workflow(WorkflowPath.NoId, Vector(
        Fail(None, Map.empty, sourcePos = sourcePos(33, 37)),
        Fail(None, Map("returnCode" -> NumberValue(7)), sourcePos = sourcePos(47, 87)),
        Fail(Some(StringConstant("ERROR")), Map.empty, sourcePos = sourcePos(97, 119)),
        Fail(Some(StringConstant("ERROR")), Map("returnCode" -> NumberValue(7)), sourcePos = sourcePos(129, 186)),
        Fail(Some(StringConstant("ERROR")), Map("returnCode" -> NumberValue(7)), uncatchable = true, sourcePos(196, 271)),
        ImplicitEnd(sourcePos = sourcePos(279, 280)))))
  }

  "lock" in {
    checkWithSourcePos("""
      define workflow {
        lock (lock="LOCK") fail;
        lock (lock="LOCK", count=3) {}
      }""",
      Workflow(WorkflowPath.NoId, Vector(
        LockInstruction(LockPath("LOCK"), None, Workflow.of(Fail(sourcePos = sourcePos(52, 56))), sourcePos = sourcePos(33, 51)),
        LockInstruction(LockPath("LOCK"), Some(3), Workflow.of(ImplicitEnd(sourcePos = sourcePos(95, 96))), sourcePos = sourcePos(66, 93)),
        ImplicitEnd(sourcePos = sourcePos(103, 104)))))
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
        execute executable="/A", agent="AGENT";
        ifFailedGoto FAILURE;
        execute executable="/B", agent="AGENT";
        goto END;
        FAILURE: execute executable="/OnFailure", agent="AGENT";
        END: end;
      }""",
    Workflow(
      WorkflowPath.NoId,
      Vector(
        Execute.Anonymous(
          WorkflowJob(AgentPath("AGENT"), PathExecutable("/A")),
          sourcePos = sourcePos(33, 71)),
        IfFailedGoto(Label("FAILURE"), sourcePos(81, 101)),
        Execute.Anonymous(
          WorkflowJob(AgentPath("AGENT"), PathExecutable("/B")),
          sourcePos = sourcePos(111, 149)),
        Goto(Label("END"), sourcePos(159, 167)),
        "FAILURE" @:
        Execute.Anonymous(
          WorkflowJob(AgentPath("AGENT"), PathExecutable("/OnFailure")),
          sourcePos = sourcePos(186, 232)),
        "END" @:
        ExplicitEnd(sourcePos(247, 250)))))
  }

  //for (n <- sys.props.get("test.speed").map(_.toInt)) "Speed" - {
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
          WorkflowJob(AgentPath("AGENT"), PathExecutable("/A")),
          sourcePos = sourcePos(90, 143)),
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
    assertEqual(parsedWorkflow.orThrow, workflow.copy(source = Some(source)))
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
