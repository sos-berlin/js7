package js7.data_for_java.order

import js7.data.agent.AgentPath
import js7.data.command.CancellationMode
import js7.data.order.{Order, OrderId, OrderMark}
import js7.data.value.StringValue
import js7.data.workflow.WorkflowPath
import js7.data.workflow.instructions.Fork
import js7.data.workflow.position.Position
import org.scalatest.freespec.AnyFreeSpec

final class JOrderTest extends AnyFreeSpec
{
  private val forkedOrder = Order(
    OrderId("ORDER-ID|A"),
    (WorkflowPath("WORKFLOW") ~ "1.0") /: (Position(1) / "fork+A" % 2),
    Order.Forked(Vector(
      Order.Forked.Child(Fork.Branch.Id("A1"), OrderId("ORDER-ID|A|A1")))),
    arguments = Map("KEY" -> StringValue("VALUE")),
    attachedState = Some(Order.Attached(AgentPath("AGENT"))),
    parent = Some(OrderId("ORDER-ID")),
    mark = Some(OrderMark.Cancelling(CancellationMode.FreshOrStarted(Some(CancellationMode.Kill())))))

  "Java" in {
    JOrderTester.testForkedOrder(JOrder(forkedOrder))
  }
}
