package js7.tests.controller.proxy;

import java.time.Instant;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;
import js7.data.event.Event;
import js7.data.order.OrderEvent;
import js7.data.order.OrderId;
import js7.data.workflow.WorkflowPath;
import js7.proxy.javaapi.JControllerApi;
import js7.proxy.javaapi.data.command.JCancelMode;
import js7.proxy.javaapi.data.controller.JEventAndControllerState;
import js7.proxy.javaapi.data.order.JFreshOrder;
import static com.google.common.collect.Maps.newHashMap;
import static java.util.Collections.singleton;
import static java.util.concurrent.TimeUnit.SECONDS;
import static js7.proxy.javaapi.data.common.VavrUtils.await;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.MatcherAssert.assertThat;

class JControllerApiOrderTester
{
    private static final WorkflowPath workflowPath = WorkflowPath.of("/WORKFLOW");  // As defined by ControllerProxyTest
    private final JControllerApi api;

    JControllerApiOrderTester(JControllerApi api) {
        this.api = api;
    }

    void testCancelOrder() throws Exception {
        boolean added = await(api.addOrder(JFreshOrder.of(
            OrderId.of("TEST-CANCEL"),
            workflowPath,
            Optional.of(Instant.parse("2100-01-01T00:00:00Z")),
            newHashMap())));
        assertThat(added, equalTo(true));

        CompletableFuture<JEventAndControllerState<Event>> cancelled =
            api.when(es -> es.stampedEvent().value().event() instanceof OrderEvent.OrderCancelled$);
        await(api.cancelOrders(singleton(OrderId.of("TEST-CANCEL")), JCancelMode.freshOrStarted()));
        cancelled.get(99, SECONDS);
    }

    void testCancelOrderViaHttpPost() {
        boolean added = await(api.addOrder(JFreshOrder.of(
            OrderId.of("TEST-CANCEL-HTTP"),
            workflowPath,
            Optional.of(Instant.parse("2100-01-01T00:00:00Z")),
            newHashMap())));
        assertThat(added, equalTo(true));

        String response = await(api
            .httpPostJson("/controller/api/command", "{'TYPE': 'CancelOrders', 'orderIds': [ 'TEST-CANCEL-HTTP' ]}"
                .replace('\'', '"')));
        assertThat(response, equalTo("{\"TYPE\":\"Accepted\"}"));
    }
}
