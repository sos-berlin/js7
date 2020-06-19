package js7.proxy.javaapi.data;

import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import js7.data.order.Order;
import js7.data.order.OrderId;
import static java.util.Arrays.asList;
import static java.util.Collections.emptyList;
import static java.util.stream.Collectors.toList;
import static js7.proxy.javaapi.data.JOrderPredicates.and;
import static js7.proxy.javaapi.data.JOrderPredicates.by;
import static js7.proxy.javaapi.data.JOrderPredicates.byOrderState;
import static js7.proxy.javaapi.data.JOrderPredicates.not;
import static js7.proxy.javaapi.data.JOrderPredicates.or;
import static js7.proxy.javaapi.data.JOrderTester.aOrder;
import static js7.proxy.javaapi.data.JOrderTester.bOrder;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;

/**
 * Java test for `JControllerState`.
 * @author Joacim Zschimmer
 */
final class JControllerStateTester
{
    static final OrderId aOrderId = OrderId.of("A-ORDER");
    private static final Map<OrderId,JOrder> expectedIdToOrder = new HashMap<>();
    static {
        expectedIdToOrder.put(aOrder.id(), aOrder);
        expectedIdToOrder.put(bOrder.id(), bOrder);
    }

    private final JControllerState controllerState;

    JControllerStateTester(JControllerState controllerState) {
        this.controllerState = controllerState;
    }

    void testWorkflows() {
        new JWorkflowTester(
            controllerState.idToWorkflow(JWorkflowId.of("/A-WORKFLOW", "COMMIT-ID")).get()
        ).test();
    }

    void testOrderIds() {
        Set<OrderId> orderIds = controllerState.orderIds();
        Set<OrderId> expected = new HashSet<>();
        expected.add(OrderId.of("A-ORDER"));
        expected.add(OrderId.of("B-ORDER"));
        assertThat(orderIds, equalTo(expected));
    }

    void testIdToOrder() {
        new JOrderTester(
            controllerState.idToOrder(aOrderId).get()
        ).test();

        assertThat(controllerState.idToOrder(OrderId.of("UNKNOWN")).isPresent(), equalTo(false));
    }

    void testEagerIdToOrder() {
        Map<OrderId,JOrder> idToOrder = controllerState.eagerIdToOrder();
        assertThat(idToOrder, equalTo(expectedIdToOrder));

        new JOrderTester(idToOrder.get(aOrderId)).test();
    }

    void testOrdersBy() {
        testOrdersBy(by(aOrder.workflowId()), asList(aOrder));
        testOrdersBy(by(bOrder.workflowId()), asList(bOrder));
        testOrdersBy(by(aOrder.workflowId().path()), asList(aOrder));
        testOrdersBy(by(bOrder.workflowId().path()), asList(bOrder));
        testOrdersBy(
            or(
                by(aOrder.workflowId().path()),
                by(bOrder.workflowId().path())),
            asList(aOrder, bOrder));
        testOrdersBy(
            and(
                by(aOrder.workflowId().path()),
                by(bOrder.workflowId().path())),
            emptyList());
        testOrdersBy(not(by(aOrder.workflowId().path())),
            asList(bOrder));
        testOrdersBy(byOrderState(Order.Fresh.class),
            asList(aOrder));
        testOrdersBy(byOrderState(Order.Ready$.class),  // Ready$, weil dieser Order.State keine Parameter hat
            asList(bOrder));
    }

    private void testOrdersBy(scala.Function1<Order<Order.State>,Object> predicate, List<JOrder> expected) {
        List<JOrder> orderList = controllerState.ordersBy(predicate).collect(toList());
        assertThat(orderList, equalTo(expected));
    }
}
