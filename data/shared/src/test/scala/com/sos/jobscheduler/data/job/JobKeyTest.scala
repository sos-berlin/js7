package com.sos.jobscheduler.data.job

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.data.workflow.WorkflowPath
import com.sos.jobscheduler.data.workflow.instructions.executable.WorkflowJob
import com.sos.jobscheduler.data.workflow.position.{Position, WorkflowBranchPath}
import com.sos.jobscheduler.tester.CirceJsonTester._
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class JobKeyTest extends AnyFreeSpec
{
  "JSON" - {
    "JobKey.Anonymous" in {
      testJson[JobKey](JobKey.Anonymous((WorkflowPath("/WORKFLOW") ~ "VERSION") /: (Position(1) / 2 % 3)),
        json"""{
          "workflowId": {
            "path": "/WORKFLOW",
            "versionId": "VERSION"
          },
          "position": [ 1, 2, 3 ]
        }""")
    }

    "JobKey.Named" in {
      testJson[JobKey](JobKey.Named(WorkflowBranchPath(WorkflowPath("/WORKFLOW") ~ "VERSION", Position(0) / 1), WorkflowJob.Name("JOBNAME")),
        json"""
      {
        "workflowId": {
          "path": "/WORKFLOW",
          "versionId": "VERSION"
        },
        "branchPath": [ 0, 1 ],
        "jobName": "JOBNAME"
      }""")
    }
  }

  "toString" in {
    assert(JobKey(WorkflowPath("/WORKFLOW") ~ "1", WorkflowJob.Name("JOBNAME")).toString == "JobKey(/WORKFLOW~1:JOBNAME)")
    assert(JobKey((WorkflowPath("/WORKFLOW") ~ "1") /: (Position(1) / "branch" % 2)).toString == "JobKey(/WORKFLOW~1:1/branch:2)")
  }
}
