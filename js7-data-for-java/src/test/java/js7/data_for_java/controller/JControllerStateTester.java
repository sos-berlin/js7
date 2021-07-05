package js7.data_for_java.controller;

import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import js7.base.web.Uri;
import js7.data.node.NodeId;
import js7.data.order.Order;
import js7.data.order.OrderId;
import js7.data.workflow.WorkflowPath;
import js7.data_for_java.cluster.JClusterState;
import js7.data_for_java.order.JOrder;
import js7.data_for_java.order.JOrderTester;
import js7.data_for_java.workflow.JWorkflowId;
import js7.data_for_java.workflow.JWorkflowTester;
import static java.util.Arrays.asList;
import static java.util.Collections.emptyList;
import static java.util.stream.Collectors.toList;
import static js7.data_for_java.order.JOrderPredicates.and;
import static js7.data_for_java.order.JOrderPredicates.by;
import static js7.data_for_java.order.JOrderPredicates.byOrderIdPredicate;
import static js7.data_for_java.order.JOrderPredicates.byOrderState;
import static js7.data_for_java.order.JOrderPredicates.byWorkflowPath;
import static js7.data_for_java.order.JOrderPredicates.markedAsDeleteWhenTerminated;
import static js7.data_for_java.order.JOrderPredicates.not;
import static js7.data_for_java.order.JOrderPredicates.or;
import static js7.data_for_java.order.JOrderTester.aOrder;
import static js7.data_for_java.order.JOrderTester.bOrder;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;

/**
 * Java test for `JControllerState`.
 * @author Joacim Zschimmer
 */
final class JControllerStateTester
{
    static final OrderId aOrderId = OrderId.of("A-ORDER");
    private static final Map<OrderId,JOrder> expectedIdToOrder = new HashMap<OrderId, JOrder>(){{
        put(aOrder.id(), aOrder);
        put(bOrder.id(), bOrder);
    }};

    private final JControllerState controllerState;

    JControllerStateTester(JControllerState controllerState) {
        this.controllerState = controllerState;
    }

    void testClusterState() {
        JClusterState.Coupled clusterState = (JClusterState.Coupled)controllerState.clusterState();
        assertThat(clusterState.idToUri(), equalTo(
                new HashMap<NodeId, Uri>(){{
                    put(NodeId.of("A"), Uri.of("http://A"));
                    put(NodeId.of("B"), Uri.of("http://B"));
                }}));
        assertThat(clusterState.activeId(), equalTo(NodeId.of("A")));
    }

    void testWorkflows() {
        new JWorkflowTester(
            controllerState.repo().idToWorkflow(JWorkflowId.of("A-WORKFLOW", "1.0")).get()
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

        testOrdersBy(byOrderState(Order.Fresh$.class),
            asList(aOrder));

        testOrdersBy(byOrderIdPredicate(orderId -> orderId.string().startsWith("B-")),
            asList(bOrder));

        testOrdersBy(markedAsDeleteWhenTerminated(false), asList(aOrder));
        testOrdersBy(markedAsDeleteWhenTerminated(true), asList(bOrder));

        testOrdersBy(controllerState.orderIsInCurrentVersionWorkflow(), asList(aOrder));
        testOrdersBy(not(controllerState.orderIsInCurrentVersionWorkflow()), asList(bOrder));
    }



    void testOrderStateToCount() {
        Map<Class<? extends Order.State>,Integer> allCounters = controllerState.orderStateToCount();
        assertThat(allCounters, equalTo(
            new HashMap<Class<? extends Order.State>, Integer>() {{
                put(Order.Fresh$.class, 1);
                put(Order.Ready$.class, 1);
            }}));

        Map<Class<? extends Order.State>,Integer> workflowCounters =
            controllerState.orderStateToCount(byWorkflowPath(WorkflowPath.of("A-WORKFLOW")));
        assertThat(workflowCounters, equalTo(
            new HashMap<Class<? extends Order.State>, Integer>() {{
                put(Order.Fresh$.class, 1);
            }}));
    }

    private void testOrdersBy(scala.Function1<Order<Order.State>,Object> predicate, List<JOrder> expected) {
        List<JOrder> orderList = controllerState.ordersBy(predicate).collect(toList());
        assertThat(orderList, equalTo(expected));
    }
}
