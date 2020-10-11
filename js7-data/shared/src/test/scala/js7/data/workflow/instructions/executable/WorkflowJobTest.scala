package js7.data.workflow.instructions.executable

import js7.base.circeutils.CirceUtils._
import js7.base.generic.GenericString.EmptyStringProblem
import js7.base.problem.Problems.InvalidNameProblem
import js7.base.time.ScalaTime._
import js7.data.agent.AgentName
import js7.data.job.ExecutablePath
import js7.tester.CirceJsonTester
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class WorkflowJobTest extends AnyFreeSpec
{
  "JSON" in {
    CirceJsonTester.testJson(
      WorkflowJob(
        AgentName("AGENT"),
        ExecutablePath("/EXECUTABLE"),
        sigkillAfter = Some(10.s)),
      json"""{
        "agentName": "AGENT",
        "executable": {
          "TYPE": "ExecutablePath",
          "path":  "/EXECUTABLE"
        },
        "taskLimit": 1,
        "sigkillAfter": 10
      }""")
  }

  "Name" in {
    import WorkflowJob.Name
    assert(Name.checked(Name.Anonymous.string) == Left(EmptyStringProblem("WorkflowJob.Name")))
    assert(Name.checked("") == Left(EmptyStringProblem("WorkflowJob.Name")))
    assert(Name.checked("/path") == Left(InvalidNameProblem("WorkflowJob.Name", "/path")))  // A WorkflowJob's name must not look like a JobPath
    assert(Name.checked("TEST") == Right(Name("TEST")))
  }
}
