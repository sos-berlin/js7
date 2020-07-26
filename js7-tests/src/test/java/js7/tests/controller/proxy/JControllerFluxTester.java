package js7.tests.controller.proxy;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import js7.base.problem.Problem;
import js7.data.event.Event;
import js7.data.event.KeyedEvent;
import js7.data.event.Stamped;
import js7.data.order.OrderEvent;
import js7.data.order.OrderId;
import js7.data.workflow.WorkflowPath;
import js7.proxy.javaapi.JAdmission;
import js7.proxy.javaapi.JControllerProxy;
import js7.proxy.javaapi.JEventAndControllerState;
import js7.proxy.javaapi.JProxyContext;
import js7.proxy.javaapi.data.JControllerState;
import js7.proxy.javaapi.data.JFreshOrder;
import js7.proxy.javaapi.data.JHttpsConfig;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import reactor.core.Disposable;
import reactor.core.publisher.Flux;
import static java.util.Arrays.asList;
import static java.util.concurrent.TimeUnit.SECONDS;
import static js7.proxy.javaapi.data.JKeyedEvent.keyedEventToJson;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.MatcherAssert.assertThat;

/**
 * @author Joacim Zschimmer
 */
public class JControllerFluxTester
implements AutoCloseable
{
    private static final Logger logger = LoggerFactory.getLogger(JControllerFluxTester.class);
    private static final List<OrderId> orderIds = IntStream.rangeClosed(0, 3)
        .mapToObj(i -> OrderId.of("TEST-ORDER-" + i))
        .collect(Collectors.toList());

    private final JProxyContext context = new JProxyContext();
    private final JControllerProxy proxy;
    private final Flux<JEventAndControllerState<Event>> flux;
    private final CouplingState couplingState = new CouplingState();

    private final Set<OrderId> finishedOrders = new HashSet<>();
    private final List<KeyedEvent<OrderEvent>> events = new ArrayList<>();
    private final CompletableFuture<Void> finished = new CompletableFuture<>();

    public JControllerFluxTester(Iterable<JAdmission> admissions, JHttpsConfig httpsConfig) {
        proxy = context.newControllerProxy(admissions, httpsConfig);
        couplingState.subscribe(proxy.proxyEventBus());
        proxy.controllerEventBus().subscribe(
            asList(OrderEvent.OrderStarted$.class, OrderEvent.OrderMoved.class, OrderEvent.OrderFinished$.class),
            this::onOrderEvent);

        flux = proxy.flux()
            .doOnNext(eventAndState -> {
                Event event = eventAndState.stampedEvent().value().event();
                if (event instanceof OrderEvent) {
                    OrderId orderId = (OrderId)eventAndState.stampedEvent().value().key();
                    doSomethingWithOrderEvent(orderId, (OrderEvent)event, eventAndState.state());
                }
            });
    }

    public void close() {
        context.close();
    }

    private void onOrderEvent(Stamped<KeyedEvent<OrderEvent>> stampedEvent, JControllerState controllerState) {
        OrderId orderId = (OrderId)stampedEvent.value().key();
        if (orderIds.contains(orderId)) {
            if (orderId.equals(orderIds.get(0))) {
                events.add(stampedEvent.value());
            }
            if (stampedEvent.value().event() instanceof OrderEvent.OrderFinished$) {
                finishedOrders.add(orderId);
                if (finishedOrders.size() == orderIds.size()) {
                    finished.complete(null);
                }
            }
        }
    }

    private void doSomethingWithOrderEvent(OrderId orderId, OrderEvent event, JControllerState state) {
        if (event instanceof OrderEvent.OrderFinished$) {
            logger.info("Flux: " +  orderId + " " + event);
        }
    }

    void test(Runnable startController) throws Exception {
        Disposable subscription = flux.subscribe();

        Problem problem = couplingState.firstProblem.get(99, SECONDS);
        assertThat(problem.toString().contains("java.net.ConnectException: Connection refused"), equalTo(true));

        startController.run();
        couplingState.coupled.get(99, SECONDS);

        try {
            for (OrderId orderId: orderIds) proxy.addOrder(newOrder(orderId)).get(99, SECONDS).get();

            finished.get(99, SECONDS);
            logger.info("Collected events: " + events.stream().map(e -> e.event().toString()).collect(Collectors.joining(" · ")));

            assertThat(events.get(0).key(), equalTo(orderIds.get(0)));
            assertThat(events.get(0).event(), instanceOf(OrderEvent.OrderStarted$.class));
            assertThat(keyedEventToJson(events.get(0)), equalTo("{\"key\":\"TEST-ORDER-0\",\"TYPE\":\"OrderStarted\"}"));

            assertThat(events.get(1).key(), equalTo(orderIds.get(0)));
            assertThat(events.get(1).event(), instanceOf(OrderEvent.OrderMoved.class));
            assertThat(keyedEventToJson(events.get(1)), equalTo("{\"key\":\"TEST-ORDER-0\",\"TYPE\":\"OrderMoved\",\"to\":[1]}"));

            assertThat(events.get(2).key(), equalTo(orderIds.get(0)));
            assertThat(events.get(2).event(), instanceOf(OrderEvent.OrderFinished$.class));
            assertThat(keyedEventToJson(events.get(2)), equalTo("{\"key\":\"TEST-ORDER-0\",\"TYPE\":\"OrderFinished\"}"));
        } finally {
            subscription.dispose();
        }
    }

    void testReusage() throws Throwable {
        CompletableFuture<Throwable> whenThrown = new CompletableFuture<>();
        flux.subscribe(x -> {}, whenThrown::complete);
        whenThrown.get(99, SECONDS);

        try {
            proxy.flux();
            throw new AssertionError("Second proxy.flux() should have failed");
        } catch (RuntimeException e) {}
    }

    private static JFreshOrder newOrder(OrderId orderId) {
        return JFreshOrder.of(orderId, WorkflowPath.of("/WORKFLOW"));
    }
}
