package com.sos.jobscheduler.tests.master.proxy;

import com.sos.jobscheduler.data.event.KeyedEvent;
import com.sos.jobscheduler.data.event.Stamped;
import com.sos.jobscheduler.data.order.OrderEvent;
import com.sos.jobscheduler.data.order.OrderEvent.OrderFinished$;
import com.sos.jobscheduler.data.order.OrderEvent.OrderMoved;
import com.sos.jobscheduler.data.order.OrderEvent.OrderStarted$;
import com.sos.jobscheduler.data.order.OrderId;
import com.sos.jobscheduler.data.workflow.WorkflowPath;
import com.sos.jobscheduler.proxy.javaapi.JCredentials;
import com.sos.jobscheduler.proxy.javaapi.JMasterProxy;
import com.sos.jobscheduler.proxy.javaapi.data.JFreshOrder;
import com.sos.jobscheduler.proxy.javaapi.data.JMasterCommand;
import com.sos.jobscheduler.proxy.javaapi.data.JMasterState;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import static com.sos.jobscheduler.proxy.javaapi.data.JKeyedEvent.keyedEventToJson;
import static com.sos.jobscheduler.proxy.javaapi.utils.VavrUtils.getOrThrowProblem;
import static java.util.Arrays.asList;
import static java.util.concurrent.TimeUnit.SECONDS;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.MatcherAssert.assertThat;

/**
 * @author Joacim Zschimmer
 */
final class JMasterProxyTester
{
    private static final OrderId orderId = OrderId.of("TEST-ORDER");
    private final JMasterProxy proxy;
    private final List<KeyedEvent<OrderEvent>> events = new ArrayList<>();
    private final CompletableFuture<Void> finished = new CompletableFuture<>();

    JMasterProxyTester(JMasterProxy proxy) {
        this.proxy = proxy;

        proxy.eventBus().subscribe(
            asList(OrderStarted$.class, OrderMoved.class, OrderFinished$.class),
            this::onOrderEvent);
    }

    private void onOrderEvent(Stamped<KeyedEvent<OrderEvent>> stampedEvent, JMasterState masterState) {
        if (stampedEvent.value().key().equals(orderId)) {
            events.add(stampedEvent.value());
            if (stampedEvent.value().event() instanceof OrderFinished$) {
                finished.complete(null);
            }
        }
    }

    static CompletableFuture<JMasterProxyTester> start(String uri, JCredentials credentials) {
        return
            JMasterProxy.start(uri, credentials)
                .thenApply(JMasterProxyTester::new);
    }

    CompletableFuture<Void> stop() {
        return proxy.stop();
    }

    void test() throws Exception {
        //MasterCommand.AddOrder.Response addOrderResponse = (MasterCommand.AddOrder.Response)
        //    proxy.executeCommand(
        //        JMasterCommand.addOrder(
        //            JFreshOrder.of(
        //                orderId,
        //                WorkflowPath.of("/WORKFLOW"),
        //                java.util.Optional.empty(),
        //                java.util.Collections.emptyMap())))
        //        .get()
        //        .get();
        String responseJson = getOrThrowProblem(
            proxy.executeCommandJson(
                JMasterCommand.addOrder(
                    JFreshOrder.of(
                        orderId,
                        WorkflowPath.of("/WORKFLOW"),
                        java.util.Optional.empty(),
                        java.util.Collections.emptyMap())
                ).toJson()
            ).get(99, SECONDS));
        assertThat(responseJson, equalTo(
            "{\"TYPE\":\"AddOrder.Response\",\"ignoredBecauseDuplicate\":false}"));

        finished.get(99, SECONDS);
        assertThat(events.get(0).key(), equalTo(orderId));
        assertThat(events.get(0).event(), instanceOf(OrderStarted$.class));
        assertThat(keyedEventToJson(events.get(0)), equalTo("{\"key\":\"TEST-ORDER\",\"TYPE\":\"OrderStarted\"}"));

        assertThat(events.get(1).key(), equalTo(orderId));
        assertThat(events.get(1).event(), instanceOf(OrderMoved.class));
        assertThat(keyedEventToJson(events.get(1)), equalTo("{\"key\":\"TEST-ORDER\",\"TYPE\":\"OrderMoved\",\"to\":[1]}"));

        assertThat(events.get(2).key(), equalTo(orderId));
        assertThat(events.get(2).event(), instanceOf(OrderFinished$.class));
        assertThat(keyedEventToJson(events.get(2)), equalTo("{\"key\":\"TEST-ORDER\",\"TYPE\":\"OrderFinished\"}"));
    }
}
