package js7.proxy.javaapi.data.order

import js7.base.process.ProcessSignal.SIGTERM
import js7.data.agent.AgentRefPath
import js7.data.command.CancelMode
import js7.data.order.{Order, OrderId}
import js7.data.workflow.WorkflowPath
import js7.data.workflow.instructions.Fork
import js7.data.workflow.position.Position
import org.scalatest.freespec.AnyFreeSpec

final class JOrderTest extends AnyFreeSpec
{
  private val forkedOrder = Order(
    OrderId("ORDER-ID/A"),
    (WorkflowPath("/WORKFLOW") ~ "1.0") /: (Position(1) / "fork+A" % 2),
    Order.Forked(Seq(
      Order.Forked.Child(Fork.Branch.Id("A1"), OrderId("ORDER-ID/A/A1")))),
    arguments = Map("KEY" -> "VALUE"),
    attachedState = Some(Order.Attached(AgentRefPath("/AGENT"))),
    parent = Some(OrderId("ORDER-ID")),
    cancel = Some(CancelMode.FreshOrStarted(Some(CancelMode.Kill(SIGTERM)))))

  "Java" in {
    JOrderTester.testForkedOrder(JOrder(forkedOrder))
  }
}
