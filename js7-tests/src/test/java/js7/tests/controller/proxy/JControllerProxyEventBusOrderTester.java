package js7.tests.controller.proxy;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import js7.data.event.KeyedEvent;
import js7.data.event.Stamped;
import js7.data.order.OrderEvent;
import js7.data.order.OrderId;
import js7.data.workflow.WorkflowPath;
import js7.data_for_java.controller.JControllerState;
import js7.data_for_java.order.JFreshOrder;
import js7.proxy.javaapi.JControllerApi;
import js7.proxy.javaapi.JControllerProxy;
import js7.proxy.javaapi.eventbus.EventSubscription;
import reactor.core.publisher.Flux;
import static java.util.Arrays.asList;
import static java.util.Collections.singleton;
import static java.util.concurrent.TimeUnit.SECONDS;
import static js7.data_for_java.event.JKeyedEvent.keyedEventToJson;
import static js7.data_for_java.vavr.VavrUtils.await;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.MatcherAssert.assertThat;

/**
 * @author Joacim Zschimmer
 */
class JControllerProxyEventBusOrderTester implements AutoCloseable
{
    private static final WorkflowPath workflowPath = WorkflowPath.of("WORKFLOW");  // As defined by ControllerProxyTest
    private static final List<OrderId> orderIds = IntStream.rangeClosed(0, 2)
        .mapToObj(i -> OrderId.of("TEST-ORDER-" + i))
        .collect(Collectors.toList());

    private final JControllerApi api;

    JControllerProxyEventBusOrderTester(JControllerProxy proxy) {
        this.api = proxy.api();
        eventSubscriptions.add(
            proxy.controllerEventBus().subscribe(
                asList(OrderEvent.OrderStarted$.class, OrderEvent.OrderMoved.class, OrderEvent.OrderFinished$.class,
                    OrderEvent.OrderDeleted$.class),
                this::onOrderEvent));
    }

    public void close() {
        for (EventSubscription o: eventSubscriptions) o.close();
    }

    private final Set<OrderId> deletedOrders = new HashSet<>();
    private final List<EventSubscription> eventSubscriptions = new ArrayList<>();
    private final List<KeyedEvent<OrderEvent>> events = new ArrayList<>();
    private final CompletableFuture<Void> allOrdersDeleted = new CompletableFuture<>();

    private void onOrderEvent(Stamped<KeyedEvent<OrderEvent>> stampedEvent, JControllerState controllerState) {
        OrderId orderId = (OrderId)stampedEvent.value().key();
        if (orderIds.contains(orderId)) {
            if (orderId.equals(orderIds.get(0))) {
                events.add(stampedEvent.value());
            }
            if (stampedEvent.value().event() instanceof OrderEvent.OrderDeleted$) {
                deletedOrders.add(orderId);
                if (deletedOrders.size() == orderIds.size()) {
                    allOrdersDeleted.complete(null);
                }
            }
        }
    }

    void testRunOrders() throws InterruptedException, ExecutionException, TimeoutException {
        // TWO WAYS TO ADD AN ORDER (first way is recommended)
        addOrdersAsStream();
        addSingleOrder();

        allOrdersDeleted.get(99, SECONDS);

        // Check events of the first added order
        assertThat(events.get(0).key(), equalTo(orderIds.get(0)));
        assertThat(events.get(0).event(), instanceOf(OrderEvent.OrderStarted$.class));
        assertThat(keyedEventToJson(events.get(0)), equalTo("{\"Key\":\"TEST-ORDER-0\",\"TYPE\":\"OrderStarted\"}"));

        assertThat(events.get(1).key(), equalTo(orderIds.get(0)));
        assertThat(events.get(1).event(), instanceOf(OrderEvent.OrderMoved.class));
        assertThat(keyedEventToJson(events.get(1)), equalTo("{\"Key\":\"TEST-ORDER-0\",\"TYPE\":\"OrderMoved\",\"to\":[1]}"));

        assertThat(events.get(2).key(), equalTo(orderIds.get(0)));
        assertThat(events.get(2).event(), instanceOf(OrderEvent.OrderFinished$.class));
        assertThat(keyedEventToJson(events.get(2)), equalTo("{\"Key\":\"TEST-ORDER-0\",\"TYPE\":\"OrderFinished\"}"));

        assertThat(events.get(3).key(), equalTo(orderIds.get(0)));
        assertThat(events.get(3).event(), instanceOf(OrderEvent.OrderDeleted$.class));
        assertThat(keyedEventToJson(events.get(3)), equalTo("{\"Key\":\"TEST-ORDER-0\",\"TYPE\":\"OrderDeleted\"}"));
    }

    private void addOrdersAsStream() {
        // Idempotent: already existent order (with same OrderId) are silently ignored
        List<JFreshOrder> freshOrders = asList(newOrder(0), newOrder(1));
        await(api.addOrders(Flux.fromIterable(freshOrders)));
        await(api.deleteOrdersWhenTerminated(freshOrders.stream().map(JFreshOrder::id).collect(Collectors.toList())));
    }

    private void addSingleOrder() {
        // #2 addOrder
        JFreshOrder freshOrder = newOrder(2);
        Boolean addOrderResponse = await(api.addOrder(freshOrder));
        assertThat(addOrderResponse, equalTo(true/*added, no duplicate*/));
        await(api.deleteOrdersWhenTerminated(singleton(freshOrder.id())));
    }

    private static JFreshOrder newOrder(int index) {
        return JFreshOrder.of(orderIds.get(index), workflowPath);
    }
}
