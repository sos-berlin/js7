package js7.tests.controller.proxy;

import io.vavr.control.Either;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;
import java.util.stream.IntStream;
import js7.base.problem.Problem;
import js7.data.controller.ControllerCommand;
import js7.data.event.KeyedEvent;
import js7.data.event.Stamped;
import js7.data.order.OrderEvent;
import js7.data.order.OrderId;
import js7.data.workflow.WorkflowPath;
import js7.data_for_java.controller.JControllerState;
import js7.data_for_java.order.JFreshOrder;
import js7.data_for_java.order.JOrder;
import js7.proxy.javaapi.JControllerApi;
import js7.proxy.javaapi.JControllerProxy;
import js7.proxy.javaapi.eventbus.EventSubscription;
import org.hamcrest.Matchers;
import reactor.core.publisher.Flux;
import static java.util.Arrays.asList;
import static java.util.concurrent.TimeUnit.SECONDS;
import static java.util.stream.Collectors.toList;
import static java.util.stream.Collectors.toMap;
import static java.util.stream.Collectors.toSet;
import static js7.data_for_java.event.JKeyedEvent.keyedEventToJson;
import static js7.data_for_java.order.JOrderPredicates.and;
import static js7.data_for_java.order.JOrderPredicates.byOrderIdPredicate;
import static js7.data_for_java.order.JOrderPredicates.markedAsDeleteWhenTerminated;
import static js7.data_for_java.vavr.VavrUtils.await;
import static js7.data_for_java.vavr.VavrUtils.getOrThrow;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.MatcherAssert.assertThat;

final class JControllerProxyAddOrderIdempotentlyTester implements AutoCloseable
{
    private static final WorkflowPath workflowPath = WorkflowPath.of("WORKFLOW");  // As defined by ControllerProxyTest
    private static final List<OrderId> orderIds = IntStream.rangeClosed(0, 2)
        .mapToObj(i -> OrderId.of("MY-ORDER-" + i))
        .collect(toList());

    private final JControllerApi api;
    private final JControllerProxy proxy;
    private final TestOrderSource orderWatch = new TestOrderSource();

    JControllerProxyAddOrderIdempotentlyTester(JControllerProxy proxy) {
        this.api = proxy.api();
        this.proxy = proxy;
        eventSubscriptions.add(
            this.proxy.controllerEventBus().subscribe(
                asList(OrderEvent.OrderStarted$.class, OrderEvent.OrderMoved.class, OrderEvent.OrderFinished.class,
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
        assertThat(orderWatch.nextOrderCount(), equalTo(3));
        addOrdersIdempotently();
        assertThat(orderWatch.nextOrderCount(), equalTo(0));

        addOrdersIdempotently();
        addOrdersIdempotently();

        allOrdersDeleted.get(99, SECONDS);

        // Check events of the first added order
        assertThat(events.get(0).key(), equalTo(orderIds.get(0)));
        assertThat(events.get(0).event(), instanceOf(OrderEvent.OrderStarted$.class));
        assertThat(keyedEventToJson(events.get(0)), equalTo("{\"Key\":\"MY-ORDER-0\",\"TYPE\":\"OrderStarted\"}"));

        assertThat(events.get(1).key(), equalTo(orderIds.get(0)));
        assertThat(events.get(1).event(), instanceOf(OrderEvent.OrderMoved.class));
        assertThat(keyedEventToJson(events.get(1)), equalTo("{\"Key\":\"MY-ORDER-0\",\"TYPE\":\"OrderMoved\",\"to\":[1]}"));

        assertThat(events.get(2).key(), equalTo(orderIds.get(0)));
        assertThat(events.get(2).event(), instanceOf(OrderEvent.OrderFinished.class));
        assertThat(keyedEventToJson(events.get(2)), equalTo("{\"Key\":\"MY-ORDER-0\",\"TYPE\":\"OrderFinished\"}"));

        assertThat(events.get(3).key(), equalTo(orderIds.get(0)));
        assertThat(events.get(3).event(), instanceOf(OrderEvent.OrderDeleted$.class));
        assertThat(keyedEventToJson(events.get(3)), equalTo("{\"Key\":\"MY-ORDER-0\",\"TYPE\":\"OrderDeleted\"}"));
    }

    private void addOrdersIdempotently() {
        List<JFreshOrder> freshOrders = orderWatch.nextOrders();

        // Using proxy.addOrders (instead of api.addOrders) synchronizes the proxy so that it mirrors the added events.
        await(proxy.addOrders(Flux.fromIterable(freshOrders)));

        List<OrderId> activeOrderIds = proxy.currentState()
            .ordersBy(and(
                markedAsDeleteWhenTerminated(false),
                byOrderIdPredicate(orderId -> orderId.string().startsWith("MY-"))))
            .map(JOrder::id)
            .collect(toList());

        assertThat(new HashSet<>(activeOrderIds), equalTo(
            orderWatch.nextOrders().stream().map(o -> o.id()).collect(toSet())));

        orderWatch.markOrdersAsAdded(activeOrderIds);

        await(api.deleteOrdersWhenTerminated(activeOrderIds));
    }

    private void checkTypesOnly() throws InterruptedException, ExecutionException, TimeoutException {
        CompletableFuture<Either<Problem,ControllerCommand.AddOrders.AddOrdersResponse>> eitherCompletableFuture =
            proxy.addOrders(Flux.empty());
        Either<Problem,ControllerCommand.AddOrders.AddOrdersResponse> checkedResponse =
            eitherCompletableFuture.get(99, SECONDS);
        ControllerCommand.AddOrders.AddOrdersResponse response = getOrThrow(checkedResponse);
        assertThat(response.eventId(), Matchers.greaterThan(0L));
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

        void markOrdersAsAdded(Iterable<OrderId> orderIds) {
            for (OrderId orderId: orderIds) orders.remove(orderId);
        }
    }
}
