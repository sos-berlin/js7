package js7.tests.master.proxy;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import js7.data.event.KeyedEvent;
import js7.data.event.Stamped;
import js7.data.order.OrderEvent;
import js7.data.order.OrderEvent.OrderFinished$;
import js7.data.order.OrderEvent.OrderMoved;
import js7.data.order.OrderEvent.OrderStarted$;
import js7.data.order.OrderId;
import js7.data.workflow.WorkflowPath;
import js7.proxy.javaapi.JCredentials;
import js7.proxy.javaapi.JMasterProxy;
import js7.proxy.javaapi.data.JFreshOrder;
import js7.proxy.javaapi.data.JHttpsConfig;
import js7.proxy.javaapi.data.JMasterCommand;
import js7.proxy.javaapi.data.JMasterState;
import static java.util.Arrays.asList;
import static java.util.concurrent.TimeUnit.SECONDS;
import static js7.proxy.javaapi.data.JKeyedEvent.keyedEventToJson;
import static js7.proxy.javaapi.utils.VavrUtils.getOrThrowProblem;
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

    private JMasterProxyTester(JMasterProxy proxy) {
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

    static CompletableFuture<JMasterProxyTester> start(String uri, JCredentials credentials, JHttpsConfig httpsConfig) {
        return
            JMasterProxy.start(uri, credentials, httpsConfig)
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
