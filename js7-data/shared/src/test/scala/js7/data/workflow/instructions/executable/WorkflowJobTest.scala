package js7.data.workflow.instructions.executable

import js7.base.circeutils.CirceUtils._
import js7.base.generic.GenericString.EmptyStringProblem
import js7.base.problem.Problem
import js7.base.problem.Problems.InvalidNameProblem
import js7.base.time.ScalaTime._
import js7.data.agent.AgentRefPath
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
        AgentRefPath("/AGENT"),
        ExecutablePath("/EXECUTABLE"),
        sigkillAfter = Some(10.s)),
      json"""{
        "agentRefPath": "/AGENT",
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

  "Anonymous AgentRef is rejected" in {
    assert(WorkflowJob.checked(AgentRefPath.Anonymous, ExecutablePath("/EXECUTABLE")) ==
      Left(Problem("Anonymous AgentRef in Job?")))
  }
}
