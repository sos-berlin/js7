package com.sos.jobscheduler.data.job

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.data.workflow.WorkflowPath
import com.sos.jobscheduler.data.workflow.instructions.executable.WorkflowJob
import com.sos.jobscheduler.data.workflow.position.{Position, WorkflowBranchPath}
import com.sos.jobscheduler.tester.CirceJsonTester._
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class JobKeyTest extends FreeSpec
{
  "JSON" - {
    "JobKey.Anonymous" in {
      testJson[JobKey](JobKey.Anonymous((WorkflowPath("/WORKFLOW") % "VERSION") /: (Position(1) / 2 % 3)),
        json"""{
          "workflowId": {
            "path": "/WORKFLOW",
            "versionId": "VERSION"
          },
          "position": [ 1, 2, 3 ]
        }""")
    }

    "JobKey.Named" in {
      testJson[JobKey](JobKey.Named(WorkflowBranchPath(WorkflowPath("/WORKFLOW") % "VERSION", Position(0) / 1), WorkflowJob.Name("NAME")),
        json"""
      {
        "workflowId": {
          "path": "/WORKFLOW",
          "versionId": "VERSION"
        },
        "branchPath": [ 0, 1 ],
        "name": "NAME"
      }""")
    }
  }
}
