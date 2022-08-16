package js7.data_for_java.order

import js7.base.test.Test
import js7.data.order.{Order, OrderId}
import js7.data.workflow.WorkflowPath

/**
  * @author Joacim Zschimmer
  */
final class JOrderPredicatesTest extends Test
{
  "JOrderPredicates" in {
    new JOrderPredicatesTester(
      Order(OrderId("A-ORDER"), WorkflowPath("WORKFLOW") ~ "0.1", Order.Fresh),
      Order(OrderId("B-ORDER"), WorkflowPath("WORKFLOW") ~ "0.1", Order.Fresh)
    ).test()
  }
}
