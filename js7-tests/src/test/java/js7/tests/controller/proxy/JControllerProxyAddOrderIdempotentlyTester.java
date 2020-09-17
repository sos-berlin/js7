package js7.tests.controller.proxy;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
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
import js7.proxy.javaapi.JControllerApi;
import js7.proxy.javaapi.JControllerProxy;
import js7.proxy.javaapi.data.controller.JControllerState;
import js7.proxy.javaapi.data.order.JFreshOrder;
import js7.proxy.javaapi.data.order.JOrder;
import js7.proxy.javaapi.eventbus.EventSubscription;
import reactor.core.publisher.Flux;
import static java.util.Arrays.asList;
import static java.util.concurrent.TimeUnit.SECONDS;
import static java.util.stream.Collectors.toMap;
import static java.util.stream.Collectors.toSet;
import static js7.proxy.javaapi.data.common.VavrUtils.getOrThrow;
import static js7.proxy.javaapi.data.event.JKeyedEvent.keyedEventToJson;
import static js7.proxy.javaapi.data.order.JOrderPredicates.and;
import static js7.proxy.javaapi.data.order.JOrderPredicates.byOrderIdPredicate;
import static js7.proxy.javaapi.data.order.JOrderPredicates.markedAsRemoveWhenTerminated;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.MatcherAssert.assertThat;

final class JControllerProxyAddOrderIdempotentlyTester implements AutoCloseable
{
    private static final WorkflowPath workflowPath = WorkflowPath.of("/WORKFLOW");  // As defined by ControllerProxyTest
    private static final List<OrderId> orderIds = IntStream.rangeClosed(0, 2)
        .mapToObj(i -> OrderId.of("MY-ORDER-" + i))
        .collect(Collectors.toList());

    private final JControllerApi api;
    private final JControllerProxy proxy;
    private final TestOrderSource orderSource = new TestOrderSource();

    JControllerProxyAddOrderIdempotentlyTester(JControllerProxy proxy) {
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
        assertThat(orderSource.nextOrderCount(), equalTo(3));
        addOrdersIdempotently();
        assertThat(orderSource.nextOrderCount(), equalTo(0));

        addOrdersIdempotently();
        addOrdersIdempotently();

        allOrdersRemoved.get(99, SECONDS);

        // Check events of the first added order
        assertThat(events.get(0).key(), equalTo(orderIds.get(0)));
        assertThat(events.get(0).event(), instanceOf(OrderEvent.OrderStarted$.class));
        assertThat(keyedEventToJson(events.get(0)), equalTo("{\"key\":\"MY-ORDER-0\",\"TYPE\":\"OrderStarted\"}"));

        assertThat(events.get(1).key(), equalTo(orderIds.get(0)));
        assertThat(events.get(1).event(), instanceOf(OrderEvent.OrderMoved.class));
        assertThat(keyedEventToJson(events.get(1)), equalTo("{\"key\":\"MY-ORDER-0\",\"TYPE\":\"OrderMoved\",\"to\":[1]}"));

        assertThat(events.get(2).key(), equalTo(orderIds.get(0)));
        assertThat(events.get(2).event(), instanceOf(OrderEvent.OrderFinished$.class));
        assertThat(keyedEventToJson(events.get(2)), equalTo("{\"key\":\"MY-ORDER-0\",\"TYPE\":\"OrderFinished\"}"));

        assertThat(events.get(3).key(), equalTo(orderIds.get(0)));
        assertThat(events.get(3).event(), instanceOf(OrderEvent.OrderRemoved$.class));
        assertThat(keyedEventToJson(events.get(3)), equalTo("{\"key\":\"MY-ORDER-0\",\"TYPE\":\"OrderRemoved\"}"));
    }

    private void addOrdersIdempotently() throws InterruptedException, ExecutionException, TimeoutException {
        List<JFreshOrder> freshOrders = orderSource.nextOrders();
        getOrThrow(proxy
            .addOrders(Flux.fromIterable(freshOrders))
            .get(99, SECONDS));

        Set<OrderId> activeOrderIds = proxy.currentState()
            .ordersBy(and(
                markedAsRemoveWhenTerminated(false),
                byOrderIdPredicate(orderId -> orderId.string().startsWith("MY-"))))
            .map(JOrder::id)
            .collect(toSet());

        assertThat(activeOrderIds, equalTo(orderSource.nextOrders().stream().map(o -> o.id()).collect(toSet())));

        orderSource.markOrdersAsAdded(activeOrderIds);

        getOrThrow(api
            .removeOrdersWhenTerminated(activeOrderIds)
            .get(99, SECONDS));
    }

    private static class TestOrderSource {
        private final Map<OrderId, JFreshOrder> orders;

        private TestOrderSource() {
            this.orders = orderIds.stream()
                .map(orderId -> JFreshOrder.of(orderId, workflowPath))
                .collect(toMap(o -> o.id(), o -> o));
        }

        List<JFreshOrder> nextOrders() {
            return new ArrayList<>(orders.values());
        }

        int nextOrderCount() {
            return orders.size();
        }

        void markOrdersAsAdded(Set<OrderId> orderIds) {
            for (OrderId orderId: orderIds) orders.remove(orderId);
        }
    }
}
