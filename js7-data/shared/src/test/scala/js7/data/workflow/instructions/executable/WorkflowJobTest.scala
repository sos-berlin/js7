package js7.data.workflow.instructions.executable

import java.time.DayOfWeek.{MONDAY, TUESDAY}
import java.time.LocalTime
import js7.base.circeutils.CirceUtils.*
import js7.base.generic.GenericString.EmptyStringProblem
import js7.base.io.process.ReturnCode
import js7.base.problem.Problems.InvalidNameProblem
import js7.base.test.OurTestSuite
import js7.base.time.ScalaTime.*
import js7.base.time.{AdmissionTimeScheme, WeekdayPeriod}
import js7.base.utils.RangeSet
import js7.data.agent.AgentPath
import js7.data.job.{JobResourcePath, RelativePathExecutable, ReturnCodeMeaning}
import js7.data.value.expression.Expression.{NumericConstant, StringConstant}
import js7.data.value.expression.ExpressionParser.expr
import js7.tester.CirceJsonTester.{testJson, testJsonDecoder}

/**
  * @author Joacim Zschimmer
  */
final class WorkflowJobTest extends OurTestSuite:

  "JSON" - {
    "default" in:
      testJson(
        WorkflowJob(
          AgentPath("AGENT"),
          RelativePathExecutable("EXECUTABLE")),
        json"""{
          "agentPath": "AGENT",
          "executable": {
            "TYPE": "PathExecutable",
            "path": "EXECUTABLE"
          },
          "processLimit": 1
        }""")

      // COMPATIBLE with 2.5.5, 2.6.2
      testJsonDecoder(
        WorkflowJob(
          AgentPath("AGENT"),
          RelativePathExecutable("EXECUTABLE")),
        json"""{
          "agentPath": "AGENT",
          "executable": {
            "TYPE": "PathExecutable",
            "path": "EXECUTABLE"
          },
          "paralellism": 1
        }""")

    "complete" in:
      testJson(
        WorkflowJob(
          AgentPath("AGENT"),
          RelativePathExecutable(
            "EXECUTABLE",
            returnCodeMeaning = ReturnCodeMeaning.Success(RangeSet(
              ReturnCode(0), ReturnCode(1)))),
          Map(
            "NAME" -> StringConstant("VALUE"),
            "NUMBER" -> NumericConstant(7)),
          Some(expr("'BUNDLE'")),
          Seq(JobResourcePath("JOB-RESOURCE")),
          processLimit = 3,
          Some(10.s),
          Some(60.s),
          killAtEndOfAdmissionPeriod = true,
          failOnErrWritten = true,
          Some(AdmissionTimeScheme(Seq(
            WeekdayPeriod(MONDAY, LocalTime.of(1, 0), 1.h),
            WeekdayPeriod(TUESDAY, LocalTime.of(9, 0), 8.h)))),
          skipIfNoAdmissionStartForOrderDay = true),
        json"""{
          "agentPath": "AGENT",
          "executable": {
            "TYPE": "PathExecutable",
            "returnCodeMeaning": {
              "success": "0,1"
            },
            "path": "EXECUTABLE"
          },
          "defaultArguments": {
            "NAME": "'VALUE'",
            "NUMBER": "7"
          },
          "subagentBundleIdExpr": "'BUNDLE'",
          "jobResourcePaths": [
            "JOB-RESOURCE"
          ],
          "processLimit": 3,
          "sigkillDelay": 10,
          "timeout": 60,
          "killAtEndOfAdmissionPeriod": true,
          "failOnErrWritten": true,
          "admissionTimeScheme": {
            "periods": [
              {
                "TYPE": "WeekdayPeriod",
                "secondOfWeek": 3600,
                "duration": 3600
              }, {
                "TYPE": "WeekdayPeriod",
                "secondOfWeek": 118800,
                "duration": 28800
              }
            ]
          },
          "skipIfNoAdmissionStartForOrderDay": true
        }""")

    "Compatible with v2.4" in:
      testJsonDecoder(
        WorkflowJob(
          AgentPath("AGENT"),
          RelativePathExecutable("EXECUTABLE"),
          subagentBundleId = Some(expr("'BUNDLE'")),
          skipIfNoAdmissionStartForOrderDay = true),
        json"""{
          "agentPath": "AGENT",
          "executable": {
            "TYPE": "PathExecutable",
            "path": "EXECUTABLE"
          },
          "subagentBundleId": "BUNDLE",
          "skipIfNoAdmissionForOrderDay": true
        }""")

    "Compatible with v2.7.1" in:
      testJsonDecoder(
        WorkflowJob(
          AgentPath("AGENT"),
          RelativePathExecutable("EXECUTABLE"),
          subagentBundleId = Some(expr("'BUNDLE'")),
          skipIfNoAdmissionStartForOrderDay = true),
        json"""{
          "agentPath": "AGENT",
          "executable": {
            "TYPE": "PathExecutable",
            "path": "EXECUTABLE"
          },
          "subagentSelectionId": "BUNDLE",
          "skipIfNoAdmissionForOrderDay": true
        }""")
  }

  "Name" in:
    import WorkflowJob.Name
    assert(Name.checked(Name.Anonymous.string) == Left(EmptyStringProblem("WorkflowJob.Name")))
    assert(Name.checked("") == Left(EmptyStringProblem("WorkflowJob.Name")))
    assert(Name.checked("/path") == Left(InvalidNameProblem("WorkflowJob.Name", "/path")))  // A WorkflowJob's name must not look like a JobPath
    assert(Name.checked("TEST") == Right(Name("TEST")))
