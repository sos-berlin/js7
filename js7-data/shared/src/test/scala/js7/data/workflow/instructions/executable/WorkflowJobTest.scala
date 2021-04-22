package js7.data.workflow.instructions.executable

import js7.base.circeutils.CirceUtils._
import js7.base.generic.GenericString.EmptyStringProblem
import js7.base.io.process.ReturnCode
import js7.base.problem.Problems.InvalidNameProblem
import js7.base.time.ScalaTime._
import js7.data.agent.AgentPath
import js7.data.job.{JobResourcePath, RelativePathExecutable}
import js7.data.value.{NumberValue, StringValue}
import js7.data.workflow.instructions.ReturnCodeMeaning
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
          "agentId": "AGENT",
          "executable": {
            "TYPE": "PathExecutable",
            "path": "EXECUTABLE"
          },
          "taskLimit": 1
        }""")
    }

    "complete" in {
      testJson(
        WorkflowJob(
          AgentPath("AGENT"),
          RelativePathExecutable("EXECUTABLE"),
          Map(
            "NAME" -> StringValue("VALUE"),
            "NUMBER" -> NumberValue(7)),
          Seq(JobResourcePath("JOB-RESOURCE")),
          ReturnCodeMeaning.Success(Set(ReturnCode(0), ReturnCode(1))),
          taskLimit = 3,
          Some(10.s),
          failOnErrWritten = true),
        json"""{
          "agentId": "AGENT",
          "executable": {
            "TYPE": "PathExecutable",
            "path": "EXECUTABLE"
          },
          "defaultArguments": {
            "NAME": "VALUE",
            "NUMBER": 7
          },
          "jobResourcePaths": [
            "JOB-RESOURCE"
          ],
          "returnCodeMeaning": {
            "success": [ 0, 1 ]
          },
          "taskLimit": 3,
          "sigkillDelay": 10,
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
