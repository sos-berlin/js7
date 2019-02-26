package com.sos.jobscheduler.data.workflow.instructions.executable

import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.data.agent.AgentRefPath
import com.sos.jobscheduler.data.job.ExecutablePath
import com.sos.jobscheduler.tester.CirceJsonTester
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class WorkflowJobTest extends FreeSpec {

  "JSON" in {
    CirceJsonTester.testJson(
      WorkflowJob(
        AgentRefPath("/AGENT"),
        ExecutablePath("/EXECUTABLE")),
      json"""{
        "agentRefPath": "/AGENT",
        "executablePath": "/EXECUTABLE",
        "taskLimit": 1
      }""")
  }

  "Name" in {
    import WorkflowJob.Name
    assert(Name.checked(Name.Anonymous.string) == Invalid(Problem("Problem with 'WorkflowJob.Name': Name must not be empty")))
    assert(Name.checked("") == Invalid(Problem("Problem with 'WorkflowJob.Name': Name must not be empty")))
    assert(Name.checked("/path") == Invalid(Problem("Problem with 'WorkflowJob.Name': Invalid character or character combination in name '/path'")))  // A WorkflowJob's name must not look like a JobPath
    assert(Name.checked("TEST") == Valid(Name("TEST")))
  }

  "Anonymous AgentRef is rejected" in {
    assert(WorkflowJob.checked(AgentRefPath.Anonymous, ExecutablePath("/EXECUTABLE")) ==
      Invalid(Problem("Anonymous AgentRef in Job?")))
  }
}
