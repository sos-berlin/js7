package js7.data.job

import js7.base.circeutils.CirceUtils._
import js7.data.workflow.WorkflowPath
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.position.{Position, WorkflowBranchPath}
import js7.tester.CirceJsonTester._
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class JobKeyTest extends AnyFreeSpec
{
  "JSON" - {
    "JobKey.Anonymous" in {
      testJson[JobKey](JobKey.Anonymous((WorkflowPath("WORKFLOW") ~ "VERSION") /: (Position(1) / "A" % 2)),
        json"""{
          "workflowId": {
            "path": "WORKFLOW",
            "versionId": "VERSION"
          },
          "position": [ 1, "A", 2 ]
        }""")
    }

    "JobKey.Named" in {
      testJson[JobKey](JobKey.Named(WorkflowBranchPath(WorkflowPath("WORKFLOW") ~ "VERSION", Position(0) / "A"), WorkflowJob.Name("JOBNAME")),
        json"""
      {
        "workflowId": {
          "path": "WORKFLOW",
          "versionId": "VERSION"
        },
        "branchPath": [ 0, "A" ],
        "jobName": "JOBNAME"
      }""")
    }
  }

  "toString" in {
    assert(JobKey(WorkflowPath("WORKFLOW") ~ "1", WorkflowJob.Name("JOBNAME")).toString == "Job:WORKFLOW~1:JOBNAME")
    assert(JobKey((WorkflowPath("WORKFLOW") ~ "1") /: (Position(1) / "branch" % 2)).toString == "Job:WORKFLOW~1:1/branch:2")
  }
}
