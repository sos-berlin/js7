package js7.tests.controller.proxy;

import java.time.Instant;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import js7.data.event.Event;
import js7.data.event.KeyedEvent;
import js7.data.event.Stamped;
import js7.data.order.OrderEvent;
import js7.data.order.OrderEvent.OrderCancelled$;
import js7.data.order.OrderId;
import js7.data.workflow.WorkflowPath;
import js7.proxy.javaapi.JControllerApi;
import js7.proxy.javaapi.JControllerProxy;
import js7.proxy.javaapi.data.command.JCancelMode;
import js7.proxy.javaapi.data.controller.JControllerState;
import js7.proxy.javaapi.data.controller.JEventAndControllerState;
import js7.proxy.javaapi.data.order.JFreshOrder;
import js7.proxy.javaapi.eventbus.EventSubscription;
import reactor.core.publisher.Flux;
import static com.google.common.collect.Maps.newHashMap;
import static java.util.Arrays.asList;
import static java.util.Collections.singleton;
import static java.util.concurrent.TimeUnit.SECONDS;
import static js7.proxy.javaapi.data.common.VavrUtils.getOrThrow;
import static js7.proxy.javaapi.data.event.JKeyedEvent.keyedEventToJson;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.MatcherAssert.assertThat;

/**
 * @author Joacim Zschimmer
 */
final class JControllerProxyOrderTester implements AutoCloseable
{
    private static final WorkflowPath workflowPath = WorkflowPath.of("/WORKFLOW");  // As defined by ControllerProxyTest
    private static final List<OrderId> orderIds = IntStream.rangeClosed(0, 2)
        .mapToObj(i -> OrderId.of("TEST-ORDER-" + i))
        .collect(Collectors.toList());

    private final JControllerApi api;
    private final JControllerProxy proxy;

    JControllerProxyOrderTester(JControllerProxy proxy) {
        this.api = proxy.api();
        this.proxy = proxy;
        eventSubscriptions.add(
            this.proxy.controllerEventBus().subscribe(
                asList(OrderEvent.OrderStarted$.class, OrderEvent.OrderMoved.class, OrderEvent.OrderFinished$.class,
                    OrderEvent.OrderRemoved$.class),
                this::onOrderEvent));
    }

    public void close() {
        for (EventSubscription o: eventSubscriptions) o.close();
    }

    private final Set<OrderId> removedOrders = new HashSet<>();
    private final List<EventSubscription> eventSubscriptions = new ArrayList<>();
    private final List<KeyedEvent<OrderEvent>> events = new ArrayList<>();
    private final CompletableFuture<Void> allOrdersRemoved = new CompletableFuture<>();

    private void onOrderEvent(Stamped<KeyedEvent<OrderEvent>> stampedEvent, JControllerState controllerState) {
        OrderId orderId = (OrderId)stampedEvent.value().key();
        if (orderIds.contains(orderId)) {
            if (orderId.equals(orderIds.get(0))) {
                events.add(stampedEvent.value());
            }
            if (stampedEvent.value().event() instanceof OrderEvent.OrderRemoved$) {
                removedOrders.add(orderId);
                if (removedOrders.size() == orderIds.size()) {
                    allOrdersRemoved.complete(null);
                }
            }
        }
    }

    void testRunOrders() throws InterruptedException, ExecutionException, TimeoutException {
        // TWO WAYS TO ADD AN ORDER (first way is recommended)
        addOrdersAsStream();
        addSingleOrder();

        allOrdersRemoved.get(99, SECONDS);

        // Check events of the first added order
        assertThat(events.get(0).key(), equalTo(orderIds.get(0)));
        assertThat(events.get(0).event(), instanceOf(OrderEvent.OrderStarted$.class));
        assertThat(keyedEventToJson(events.get(0)), equalTo("{\"key\":\"TEST-ORDER-0\",\"TYPE\":\"OrderStarted\"}"));

        assertThat(events.get(1).key(), equalTo(orderIds.get(0)));
        assertThat(events.get(1).event(), instanceOf(OrderEvent.OrderMoved.class));
        assertThat(keyedEventToJson(events.get(1)), equalTo("{\"key\":\"TEST-ORDER-0\",\"TYPE\":\"OrderMoved\",\"to\":[1]}"));

        assertThat(events.get(2).key(), equalTo(orderIds.get(0)));
        assertThat(events.get(2).event(), instanceOf(OrderEvent.OrderFinished$.class));
        assertThat(keyedEventToJson(events.get(2)), equalTo("{\"key\":\"TEST-ORDER-0\",\"TYPE\":\"OrderFinished\"}"));

        assertThat(events.get(3).key(), equalTo(orderIds.get(0)));
        assertThat(events.get(3).event(), instanceOf(OrderEvent.OrderRemoved$.class));
        assertThat(keyedEventToJson(events.get(3)), equalTo("{\"key\":\"TEST-ORDER-0\",\"TYPE\":\"OrderRemoved\"}"));
    }

    private void addOrdersAsStream() throws InterruptedException, ExecutionException, TimeoutException {
        // Idempotent: already existent order (with same OrderId) are silently ignored
        List<JFreshOrder> freshOrders = asList(newOrder(0), newOrder(1));
        getOrThrow(api
            .addOrders(Flux.fromIterable(freshOrders))
            .get(99, SECONDS));
        getOrThrow(api
            .removeOrdersWhenTerminated(freshOrders.stream().map(JFreshOrder::id).collect(Collectors.toList()))
            .get(99, SECONDS));
    }

    private void addSingleOrder() throws InterruptedException, ExecutionException, TimeoutException {
        // #2 addOrder
        JFreshOrder freshOrder = newOrder(2);
        Boolean addOrderResponse = getOrThrow(api
            .addOrder(freshOrder)
            .get(99, SECONDS));
        assertThat(addOrderResponse, equalTo(true/*added, no duplicate*/));
        getOrThrow(api
            .removeOrdersWhenTerminated(singleton(freshOrder.id()))
            .get(99, SECONDS));
    }

    private static JFreshOrder newOrder(int index) {
        return JFreshOrder.of(orderIds.get(index), workflowPath);
    }

    void testCancelOrder() throws Exception {
        boolean added = getOrThrow(api
            .addOrder(JFreshOrder.of(
                OrderId.of("TEST-CANCEL"),
                workflowPath,
                Optional.of(Instant.parse("2100-01-01T00:00:00Z")),
                newHashMap()))
            .get(99, SECONDS));
        assertThat(added, equalTo(true));

        CompletableFuture<JEventAndControllerState<Event>> cancelled =
            proxy.when(es -> es.stampedEvent().value().event() instanceof OrderCancelled$);
        getOrThrow(api
            .cancelOrders(singleton(OrderId.of("TEST-CANCEL")), JCancelMode.freshOrStarted())
            .get(99, SECONDS));
        cancelled.get(99, SECONDS);
    }

    void testCancelOrderViaHttpPost() throws Exception {
        boolean added = getOrThrow(api
            .addOrder(JFreshOrder.of(
                OrderId.of("TEST-CANCEL-HTTP"),
                workflowPath,
                Optional.of(Instant.parse("2100-01-01T00:00:00Z")),
                newHashMap()))
            .get(99, SECONDS));
        assertThat(added, equalTo(true));

        String response = getOrThrow(api
            .httpPostJson("/controller/api/command", "{'TYPE': 'CancelOrders', 'orderIds': [ 'TEST-CANCEL-HTTP' ]}"
                .replace('\'', '"'))
            .get(99, SECONDS));
        assertThat(response, equalTo("{\"TYPE\":\"Accepted\"}"));
    }
}
