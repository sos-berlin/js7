package com.sos.jobscheduler.data.workflow

import com.sos.jobscheduler.data.workflow.WorkflowRoute._
import com.sos.jobscheduler.data.workflow.test.ForkTestSetting.{A, D, END, G, TestWorkflow, a, c, g}
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class WorkflowRouteTest extends FreeSpec {

  "linearPath" - {
    "no transition" in {
      assert(Nil.linearPath(TestWorkflow.start) == Some(List(A.id)))
    }

    "single transition" in {
      assert(List(g).linearPath(G.id) == Some(List(G.id, END.id)))
    }

    "single fork/join" in {
      assert(List(a, c).linearPath(A.id) == Some(List(A.id, D.id)))
    }

    "loop" in {
      pending
    }

    "TestForkSetting" in {
      assert(TestWorkflow.route.linearPath == Some(List(A, D, G, END)))
    }
  }
}
