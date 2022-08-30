package js7.data_for_java.order

import js7.data.order.{Order, OrderId}
import js7.data.workflow.WorkflowPath
import js7.data.workflow.position.Position
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class JOrderPredicatesTest extends AnyFreeSpec
{
  "JOrderPredicates" in {
    new JOrderPredicatesTester(
      Order(OrderId("A-ORDER"), WorkflowPath("WORKFLOW") ~ "0.1" /: Position(0), Order.Fresh),
      Order(OrderId("B-ORDER"), WorkflowPath("WORKFLOW") ~ "0.1" /: Position(0), Order.Fresh)
    ).test()
  }
}
