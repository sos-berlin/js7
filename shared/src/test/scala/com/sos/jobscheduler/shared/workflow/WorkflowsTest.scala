package com.sos.jobscheduler.shared.workflow

import com.sos.jobscheduler.data.order.Order
import com.sos.jobscheduler.data.workflow.test.ForkTestSetting._
import com.sos.jobscheduler.shared.workflow.Workflows.ExecutableWorkflowGraph
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class WorkflowsTest extends FreeSpec {

  "ExecutableWorkflow" - {
    val isSwitchableSetting = List(
      ((A.id , Order.Processed), true),
      ((Bx.id, Order.Processed), true),  ((By.id, Order.Processed), true),
      ((Cx.id, Order.Processed), false), ((Cy.id, Order.Processed), false),
      ((A.id , Order.Forked(Nil)), false),

      ((D.id , Order.Processed), true),
      ((Ex.id, Order.Processed), true),   ((Ey.id, Order.Processed), true),
      ((Fx.id, Order.Processed), true),   ((Fy.id, Order.Processed), true),
      ((D.id , Order.Forked(Nil)), true),

      ((G.id , Order.Processed), false/*true would be okay*/))

    for (((nodeId, state), expected) ← isSwitchableSetting) {
      //s"isTransitionableOnAgent($nodeId $state) = $expected" in {
      //  assert(TestWorkflow.isTransitionableOnAgent(nodeId, state, AAgentPath) == expected)
      //}
      s"isTransitionableOnAgent($nodeId $state) = $expected - reduceForAgent" in {
        assert(TestWorkflow.graph.reduceForAgent(AAgentPath).isTransitionableOnAgent(nodeId, state, AAgentPath) == expected)
      }
    }

    "transitionForNode" in {
      assert(TestWorkflow.graph.transitionForNode(A.id, Order.Processed) == Some(a))
      assert(TestWorkflow.graph.transitionForNode(Bx.id, Order.Processed) == Some(bx))
      assert(TestWorkflow.graph.transitionForNode(By.id, Order.Processed) == Some(by))
      assert(TestWorkflow.graph.transitionForNode(Cx.id, Order.Processed) == Some(c))
      assert(TestWorkflow.graph.transitionForNode(Cy.id, Order.Processed) == Some(c))
      assert(TestWorkflow.graph.transitionForNode(A.id, Order.Forked(Nil)) == Some(c))

      assert(TestWorkflow.graph.transitionForNode(D.id, Order.Processed) == Some(d))
      assert(TestWorkflow.graph.transitionForNode(Ex.id, Order.Processed) == Some(ex))
      assert(TestWorkflow.graph.transitionForNode(Ey.id, Order.Processed) == Some(ey))
      assert(TestWorkflow.graph.transitionForNode(Fx.id, Order.Processed) == Some(f))
      assert(TestWorkflow.graph.transitionForNode(Fy.id, Order.Processed) == Some(f))
      assert(TestWorkflow.graph.transitionForNode(D.id, Order.Forked(Nil)) == Some(f))

      assert(TestWorkflow.graph.transitionForNode(G.id, Order.Processed) == Some(g))
    }
  }
}
