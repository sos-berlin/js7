package js7.data_for_java.controller;

import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import js7.base.web.Uri;
import js7.data.board.BoardPath;
import js7.data.board.NoticeKey;
import js7.data.board.PlannedBoardId;
import js7.data.node.NodeId;
import js7.data.order.Order;
import js7.data.order.OrderId;
import js7.data.plan.PlanId;
import js7.data.plan.PlanKey;
import js7.data.plan.PlanSchemaId;
import js7.data.workflow.WorkflowPath;
import js7.data_for_java.board.JNoticePlace;
import js7.data_for_java.board.JPlannedBoard;
import js7.data_for_java.cluster.JClusterState;
import js7.data_for_java.order.JOrder;
import js7.data_for_java.order.JOrderTester;
import js7.data_for_java.plan.JPlan;
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
                    put(NodeId.of("A"), Uri.of("https://A"));
                    put(NodeId.of("B"), Uri.of("https://B"));
                }}));
        assertThat(clusterState.activeId(), equalTo(NodeId.of("A")));
    }

    void testWorkflows() {
        new JWorkflowTester(
            controllerState.repo().idToCheckedWorkflow(JWorkflowId.of("A-WORKFLOW", "1.0")).get()
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
            controllerState.idToOrder().get(aOrderId)
        ).test();

        assertThat(controllerState.idToOrder().containsKey(OrderId.of("UNKNOWN")), equalTo(false));
    }

    void testOrdersBy() {
        testOrdersBy(by(aOrder.workflowId()), List.of(aOrder));
        testOrdersBy(by(bOrder.workflowId()), List.of(bOrder));
        testOrdersBy(by(aOrder.workflowId().path()), List.of(aOrder));
        testOrdersBy(by(bOrder.workflowId().path()), List.of(bOrder));
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
            List.of(bOrder));

        testOrdersBy(byOrderState(Order.Fresh$.class),
            List.of(aOrder));

        testOrdersBy(byOrderIdPredicate(orderId -> orderId.string().startsWith("B-")),
            List.of(bOrder));

        testOrdersBy(markedAsDeleteWhenTerminated(false), List.of(aOrder));
        testOrdersBy(markedAsDeleteWhenTerminated(true), List.of(bOrder));
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

    void testToPlan() {
        var toPlan = controllerState.toPlan();

        var expected = Map.of(
            PlanId.Global(),
            JPlan.of(
                PlanId.Global(),
                Set.of(OrderId.of("A-ORDER")),
                asList(),
                false),
            PlanId.apply(PlanSchemaId.of("DailyPlan"), PlanKey.of("2025-01-29")),
            JPlan.of(
                PlanId.apply(PlanSchemaId.of("DailyPlan"), PlanKey.of("2025-01-29")),
                Set.of(OrderId.of("B-ORDER")),
                asList(
                    JPlannedBoard.of(
                        PlannedBoardId.apply(
                            PlanId.apply(PlanSchemaId.of("DailyPlan"), PlanKey.of("2025-01-29")),
                            BoardPath.of("BOARD")),
                        Map.of(
                            NoticeKey.of("NOTICE"),
                            JNoticePlace.of(Optional.empty(), Set.of(), true, false, 0))),
                    JPlannedBoard.of(
                        PlannedBoardId.apply(
                            PlanId.apply(PlanSchemaId.of("DailyPlan"), PlanKey.of("2025-01-29")),
                            BoardPath.of("BOARD-2")),
                        Map.of(
                            NoticeKey.of("NOTICE"),
                            JNoticePlace.of(
                                Optional.empty(),
                                Set.of(OrderId.of("B-ORDER")),
                                false, false, 0)))),
                false));

        assertThat(toPlan, equalTo(expected));
    }

    private void testOrdersBy(scala.Function1<Order<Order.State>,Object> predicate, List<JOrder> expected) {
        List<JOrder> orderList = controllerState.ordersBy(predicate).collect(toList());
        assertThat(orderList, equalTo(expected));
    }
}
