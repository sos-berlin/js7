package js7.data_for_java.order

import js7.base.test.OurTestSuite
import js7.data.agent.AgentPath
import js7.data.command.CancellationMode
import js7.data.order.{Order, OrderId, OrderMark}
import js7.data.value.StringValue
import js7.data.workflow.WorkflowPath
import js7.data.workflow.instructions.ForkBranchId
import js7.data.workflow.position.BranchPath.syntax.*
import js7.data.workflow.position.Position

final class JOrderTest extends OurTestSuite:
  private val forkedOrder = Order(
    OrderId("ORDER-ID|A"),
    (WorkflowPath("WORKFLOW") ~ "1.0") /: (Position(1) / "fork+A" % 2),
    Order.Forked(Vector(
      Order.Forked.Child(ForkBranchId("A1"), OrderId("ORDER-ID|A|A1")))),
    arguments = Map("KEY" -> StringValue("VALUE")),
    attachedState = Some(Order.Attached(AgentPath("AGENT"))),
    parent = Some(OrderId("ORDER-ID")),
    mark = Some(OrderMark.Cancelling(CancellationMode.FreshOrStarted(Some(CancellationMode.Kill())))))

  "Java" in:
    JOrderTester.testForkedOrder(JOrder(forkedOrder))
