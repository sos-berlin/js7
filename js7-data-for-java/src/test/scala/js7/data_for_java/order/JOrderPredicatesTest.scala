package js7.data_for_java.order

import js7.base.test.OurTestSuite
import js7.data.order.{Order, OrderId}
import js7.data.workflow.WorkflowPath
import js7.data.workflow.position.Position

/**
  * @author Joacim Zschimmer
  */
final class JOrderPredicatesTest extends OurTestSuite
{
  "JOrderPredicates" in {
    new JOrderPredicatesTester(
      Order(OrderId("A-ORDER"), WorkflowPath("WORKFLOW") ~ "0.1" /: Position(0), Order.Fresh),
      Order(OrderId("B-ORDER"), WorkflowPath("WORKFLOW") ~ "0.1" /: Position(0), Order.Fresh)
    ).test()
  }
}
