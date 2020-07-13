package js7.tests.controller.proxy;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import js7.base.problem.Problem;
import js7.controller.data.ControllerCommand$AddOrder$Response;
import js7.data.event.KeyedEvent;
import js7.data.event.Stamped;
import js7.data.order.OrderEvent;
import js7.data.order.OrderEvent.OrderFinished$;
import js7.data.order.OrderEvent.OrderMoved;
import js7.data.order.OrderEvent.OrderStarted$;
import js7.data.order.OrderId;
import js7.data.workflow.WorkflowPath;
import js7.proxy.ProxyEvent;
import js7.proxy.ProxyEvent.ProxyCoupled;
import js7.proxy.ProxyEvent.ProxyCouplingError;
import js7.proxy.ProxyEvent.ProxyDecoupled$;
import js7.proxy.javaapi.JControllerProxy;
import js7.proxy.javaapi.JCredentials;
import js7.proxy.javaapi.JProxyContext;
import js7.proxy.javaapi.JStandardEventBus;
import js7.proxy.javaapi.data.JControllerCommand;
import js7.proxy.javaapi.data.JControllerState;
import js7.proxy.javaapi.data.JFreshOrder;
import js7.proxy.javaapi.data.JHttpsConfig;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
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
final class JControllerProxyTester
{
    private static final Logger logger = LoggerFactory.getLogger(JControllerProxyTester.class);
    private static final List<OrderId> orderIds = IntStream.rangeClosed(0, 3)
        .mapToObj(i -> OrderId.of("TEST-ORDER-" + i))
        .collect(Collectors.toList());
    private static final Set<OrderId> finishedOrders = new HashSet<>();
    private final JControllerProxy proxy;
    private final CouplingState couplingState;
    private final List<KeyedEvent<OrderEvent>> events = new ArrayList<>();
    private final CompletableFuture<Void> finished = new CompletableFuture<>();

    private JControllerProxyTester(JControllerProxy proxy, CouplingState couplingState) {
        this.proxy = proxy;
        this.couplingState = couplingState;
        proxy.controllerEventBus().subscribe(
            asList(OrderStarted$.class, OrderMoved.class, OrderFinished$.class),
            this::onOrderEvent);
    }

    private void onOrderEvent(Stamped<KeyedEvent<OrderEvent>> stampedEvent, JControllerState controllerState) {
        OrderId orderId = (OrderId)stampedEvent.value().key();
        if (orderIds.contains(orderId)) {
            if (orderId.equals(orderIds.get(0))) {
                events.add(stampedEvent.value());
            }
            if (stampedEvent.value().event() instanceof OrderFinished$) {
                finishedOrders.add(orderId);
                if (finishedOrders.size() == orderIds.size()) {
                    finished.complete(null);
                }
            }
        }
    }

    private CompletableFuture<Void> stop() {
        return CompletableFuture.allOf(
            couplingState.decoupled,
            proxy.stop());
    }

    private void test() throws Exception {
        couplingState.coupled.get();

        String overview = getOrThrowProblem(
            proxy.httpGetJson("/controller/api")
                .get(99, SECONDS));
        assertThat(overview.contains("\"id\":\"Controller\""), equalTo(true));


        // FOUR WAYS TO ADD AN ORDER (first way is recommended)

        // #0 addOrder

        Boolean addOrderResponse = proxy.addOrder(newOrder(0)).get().get();
        assertThat(addOrderResponse, equalTo(true/*added, no duplicate*/));

        // #1 JControllerCommand.addOrder
        // Red in IntelliJ IDE, but it compiles
        ControllerCommand$AddOrder$Response commandResponse0 = (ControllerCommand$AddOrder$Response)
            proxy.executeCommand(
                JControllerCommand.addOrder(newOrder(1))
            ).get().get();
        assertThat(commandResponse0.ignoredBecauseDuplicate(), equalTo(false));


        // #2 JControllerCommand.addOrder

        String commandResponse1 = getOrThrowProblem(
            proxy.executeCommandJson(
                JControllerCommand.addOrder(newOrder(2)).toJson()
            ).get(99, SECONDS));
        assertThat(commandResponse1, equalTo(
            "{\"TYPE\":\"AddOrder.Response\",\"ignoredBecauseDuplicate\":false}"));


        // #3 POST JFreshOrder
        String postResponse = getOrThrowProblem(
            proxy.httpPostJson("/controller/api/order", newOrder(3).toJson())
                .get(99, SECONDS));
        assertThat(postResponse, equalTo("{}"));


        finished.get(99, SECONDS);
        assertThat(events.get(0).key(), equalTo(orderIds.get(0)));
        assertThat(events.get(0).event(), instanceOf(OrderStarted$.class));
        assertThat(keyedEventToJson(events.get(0)), equalTo("{\"key\":\"TEST-ORDER-0\",\"TYPE\":\"OrderStarted\"}"));

        assertThat(events.get(1).key(), equalTo(orderIds.get(0)));
        assertThat(events.get(1).event(), instanceOf(OrderMoved.class));
        assertThat(keyedEventToJson(events.get(1)), equalTo("{\"key\":\"TEST-ORDER-0\",\"TYPE\":\"OrderMoved\",\"to\":[1]}"));

        assertThat(events.get(2).key(), equalTo(orderIds.get(0)));
        assertThat(events.get(2).event(), instanceOf(OrderFinished$.class));
        assertThat(keyedEventToJson(events.get(2)), equalTo("{\"key\":\"TEST-ORDER-0\",\"TYPE\":\"OrderFinished\"}"));
    }

    private JFreshOrder newOrder(int index) {
        return JFreshOrder.of(
            orderIds.get(index),
            WorkflowPath.of("/WORKFLOW"),
            java.util.Optional.empty(),
            java.util.Collections.emptyMap());
    }

    static void run(String uri, JCredentials credentials, JHttpsConfig httpsConfig,
        Runnable startController) throws Exception
    {
        JStandardEventBus<ProxyEvent> proxyEventBus = new JStandardEventBus<>(ProxyEvent.class);
        CouplingState couplingState = new CouplingState();
        proxyEventBus.subscribe(asList(ProxyCoupled.class), couplingState::onProxyCoupled);
        proxyEventBus.subscribe(asList(ProxyDecoupled$.class), couplingState::onProxyDecoupled);
        proxyEventBus.subscribe(asList(ProxyCouplingError.class), couplingState::onProxyCouplingError);

        try (JProxyContext context = new JProxyContext()) {
            CompletableFuture<JControllerProxy> whenStarted = context.startControllerProxy(uri, credentials, httpsConfig, proxyEventBus);

            Problem problem = couplingState.firstProblem.get();
            assertThat(problem.toString().contains("java.net.ConnectException: Connection refused"), equalTo(true));

            startController.run();
            JControllerProxy proxy = whenStarted.get(99, SECONDS);

            JControllerProxyTester tester = new JControllerProxyTester(proxy, couplingState);
            tester.test();

            tester.stop().get(99, SECONDS);
        }
    }

    private static final class CouplingState
    {
        final CompletableFuture<Void> coupled = new CompletableFuture<>();
        final CompletableFuture<Void> decoupled = new CompletableFuture<>();
        final CompletableFuture<Problem> firstProblem = new CompletableFuture<>();
        Optional<Problem> lastProblem = Optional.empty();

        void onProxyCoupled(ProxyCoupled proxyCoupled) {
            logger.info(proxyCoupled.toString());
            coupled.complete(null);
        }

        void onProxyDecoupled(ProxyDecoupled$ proxyDecoupled) {
            logger.info(proxyDecoupled.toString());
            decoupled.complete(null);
        }

        void onProxyCouplingError(ProxyCouplingError proxyCouplingError) {
            logger.info(proxyCouplingError.toString());
            firstProblem.complete(proxyCouplingError.problem());
            lastProblem = Optional.of(proxyCouplingError.problem());
        }
    }
}
