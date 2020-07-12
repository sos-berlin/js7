package js7.tests.controller.proxy;

import com.typesafe.config.ConfigFactory;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import js7.base.problem.Problem;
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
import js7.proxy.javaapi.JControllerEventBus;
import js7.proxy.javaapi.JCredentials;
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
    private static final OrderId orderId = OrderId.of("TEST-ORDER");
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
        if (stampedEvent.value().key().equals(orderId)) {
            events.add(stampedEvent.value());
            if (stampedEvent.value().event() instanceof OrderFinished$) {
                finished.complete(null);
            }
        }
    }

    static CompletableFuture<JControllerProxyTester> start(String uri, JCredentials credentials, JHttpsConfig httpsConfig,
        Runnable startController) throws ExecutionException, InterruptedException
    {
        JStandardEventBus<ProxyEvent> proxyEventBus = new JStandardEventBus<>(ProxyEvent.class);
        JControllerEventBus controllerEventBus = new JControllerEventBus();

        CouplingState couplingState = new CouplingState();
        proxyEventBus.subscribe(asList(ProxyCoupled.class), couplingState::onProxyCoupled);
        proxyEventBus.subscribe(asList(ProxyDecoupled$.class), couplingState::onProxyDecoupled);
        proxyEventBus.subscribe(asList(ProxyCouplingError.class), couplingState::onProxyCouplingError);
        CompletableFuture<JControllerProxyTester> whenStarted =
            JControllerProxy.start(uri, credentials, httpsConfig, proxyEventBus, controllerEventBus, ConfigFactory.empty())
                .thenApply(proxy -> new JControllerProxyTester(proxy, couplingState));
        Problem problem = couplingState.firstProblem.get();
        assertThat(problem.toString().contains("java.net.ConnectException: Connection refused"), equalTo(true));
        startController.run();
        return whenStarted;
    }

    CompletableFuture<Void> stop() {
        return CompletableFuture.allOf(
            couplingState.decoupled,
            proxy.stop());
    }

    void test() throws Exception {
        couplingState.coupled.get();

        //ControllerCommand.AddOrder.Response addOrderResponse = (ControllerCommand.AddOrder.Response)
        //    proxy.executeCommand(
        //        JControllerCommand.addOrder(
        //            JFreshOrder.of(
        //                orderId,
        //                WorkflowPath.of("/WORKFLOW"),
        //                java.util.Optional.empty(),
        //                java.util.Collections.emptyMap())))
        //        .get()
        //        .get();
        String responseJson = getOrThrowProblem(
            proxy.executeCommandJson(
                JControllerCommand.addOrder(
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
