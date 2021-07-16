package js7.data.workflow.instructions.executable

import js7.base.circeutils.CirceUtils._
import js7.base.generic.GenericString.EmptyStringProblem
import js7.base.io.process.{KeyLogin, ReturnCode}
import js7.base.problem.Problems.InvalidNameProblem
import js7.base.time.ScalaTime._
import js7.data.agent.AgentPath
import js7.data.job.{JobResourcePath, RelativePathExecutable, ReturnCodeMeaning}
import js7.data.value.expression.Expression.{NumericConstant, StringConstant}
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class WorkflowJobTest extends AnyFreeSpec
{
  "JSON" - {
    "default" in {
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
          "parallelism": 1
        }""")
    }

    "complete" in {
      testJson(
        WorkflowJob(
          AgentPath("AGENT"),
          RelativePathExecutable(
            "EXECUTABLE",
            returnCodeMeaning = ReturnCodeMeaning.Success(Set(ReturnCode(0), ReturnCode(1)))),
          Map(
            "NAME" -> StringConstant("VALUE"),
            "NUMBER" -> NumericConstant(7)),
          Seq(JobResourcePath("JOB-RESOURCE")),
          parallelism = 3,
          Some(10.s),
          Some(60.s),
          Some(KeyLogin("CREDENTIALS KEY", withUserProfile = true)),
          failOnErrWritten = true),
        json"""{
          "agentPath": "AGENT",
          "executable": {
            "TYPE": "PathExecutable",
            "returnCodeMeaning": {
              "success": [ 0, 1 ]
            },
            "path": "EXECUTABLE"
          },
          "defaultArguments": {
            "NAME": "'VALUE'",
            "NUMBER": "7"
          },
          "jobResourcePaths": [
            "JOB-RESOURCE"
          ],
          "parallelism": 3,
          "sigkillDelay": 10,
          "timeout": 60,
          "failOnErrWritten": true
        }""")
    }
  }

  "Name" in {
    import WorkflowJob.Name
    assert(Name.checked(Name.Anonymous.string) == Left(EmptyStringProblem("WorkflowJob.Name")))
    assert(Name.checked("") == Left(EmptyStringProblem("WorkflowJob.Name")))
    assert(Name.checked("/path") == Left(InvalidNameProblem("WorkflowJob.Name", "/path")))  // A WorkflowJob's name must not look like a JobPath
    assert(Name.checked("TEST") == Right(Name("TEST")))
  }
}
