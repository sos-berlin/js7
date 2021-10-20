package js7.tests.controller.proxy;

import java.time.Instant;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.CompletableFuture;
import js7.data.event.Event;
import js7.data.order.OrderEvent;
import js7.data.order.OrderId;
import js7.data.workflow.WorkflowPath;
import js7.data_for_java.command.JCancellationMode;
import js7.data_for_java.controller.JControllerState;
import js7.data_for_java.order.JFreshOrder;
import js7.data_for_java.order.JOrderObstacle;
import js7.data_for_java.order.JOrderObstacle.WaitingForTime;
import js7.proxy.javaapi.JControllerApi;
import js7.proxy.javaapi.data.controller.JEventAndControllerState;
import static com.google.common.collect.Maps.newHashMap;
import static java.util.Collections.singleton;
import static java.util.concurrent.TimeUnit.SECONDS;
import static js7.data_for_java.vavr.VavrUtils.await;
import static js7.data_for_java.vavr.VavrUtils.getOrThrow;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.MatcherAssert.assertThat;

class JControllerApiOrderTester
{
    private static final WorkflowPath workflowPath = WorkflowPath.of("WORKFLOW");  // As defined by ControllerProxyTest
    private final JControllerApi api;

    JControllerApiOrderTester(JControllerApi api) {
        this.api = api;
    }

    void testCancelOrder() throws Exception {
        OrderId orderId = OrderId.of("TEST-CANCEL");
        Instant scheduledFor = Instant.parse("2100-01-01T00:00:00Z");
        boolean added = await(api.addOrder(JFreshOrder.of(
            orderId,
            workflowPath,
            Optional.of(scheduledFor),
            newHashMap())));
        assertThat(added, equalTo(true));


        // Fetch the whole big ControllerState:
        JControllerState controllerState = await(api.controllerState());
        assertThat(controllerState.idToOrder(OrderId.of("TEST-CANCEL")).isPresent(), equalTo(true));

        // OrderObstacle
        Set<JOrderObstacle> obstacles = getOrThrow(controllerState.orderToObstacles(orderId, Instant.now()));
        JOrderObstacle obstacle = obstacles.iterator().next();
        assertThat(obstacle instanceof WaitingForTime, equalTo(true));
        assertThat(((WaitingForTime)obstacle).instant(), equalTo(scheduledFor));

        CompletableFuture<JEventAndControllerState<Event>> cancelled =
            api.when(es -> es.stampedEvent().value().event() instanceof OrderEvent.OrderCancelled$);
        await(api.cancelOrders(singleton(OrderId.of("TEST-CANCEL")), JCancellationMode.freshOrStarted()));
        cancelled.get(99, SECONDS);
    }
}
