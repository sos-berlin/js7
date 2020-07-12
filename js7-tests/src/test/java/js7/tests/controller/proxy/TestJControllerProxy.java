package js7.tests.controller.proxy;

import com.typesafe.config.ConfigFactory;
import java.time.Instant;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.stream.Collectors;
import js7.data.event.KeyedEvent;
import js7.data.event.Stamped;
import js7.data.order.OrderEvent;
import js7.data.order.OrderId;
import js7.proxy.ProxyEvent;
import js7.proxy.javaapi.JControllerProxy;
import js7.proxy.javaapi.JControllerEventBus;
import js7.proxy.javaapi.JCredentials;
import js7.proxy.javaapi.JStandardEventBus;
import js7.proxy.javaapi.data.JControllerState;
import js7.proxy.javaapi.data.JHttpsConfig;
import static java.lang.System.out;
import static java.util.Arrays.asList;

/** Example for usage of JControllerProxy.
 * @author Joacim Zschimmer
 */
public final class TestJControllerProxy implements AutoCloseable
{
    private final JControllerProxy proxy;

    private TestJControllerProxy(JControllerProxy proxy) {
        this.proxy = proxy;
    }

    public void close() {
        proxy.close();
    }

    private void run() {
        while (true) {
            out.println(controllerStateToString(proxy.currentState()));
            sleep(1000);
        }
    }

    private static String controllerStateToString(JControllerState controllerState) {
        return //Instant.ofEpochMilli(EventId.toEpochMilli(controllerState.eventId())) + " " +
            controllerState.orderIds().size() +
            " orders: " +
            controllerState.orderIds().stream()
                .limit(5)
                .map(OrderId::string)
                .collect(Collectors.joining(", "));
    }

    private static String orderEventToString(Stamped<KeyedEvent<OrderEvent>> stamped) {
        Instant timestamp = stamped.timestamp().toInstant();
        KeyedEvent<OrderEvent> event = stamped.value();
        return timestamp + " " + event;
    }

    private static void sleep(int millis) {
        try {
            Thread.sleep(millis);
        } catch(Exception e) {
            throw new RuntimeException(e);
        }
    }

    /**
     * @param args: URI [USERID PASSWORD]
     * @throws ExecutionException
     * @throws InterruptedException
     */
    public static void main(String[] args) throws ExecutionException, InterruptedException {
        if (args.length != 1 && args.length != 3) throw new IllegalArgumentException("One or three arguments required: URI USERID PASSWORD");
        String uri = args[0];
        final JCredentials credentials = args.length == 3 ?  JCredentials.of(args[1], args[2]) : JCredentials.noCredentials();
        start(uri, credentials).get();
    }

    private static CompletableFuture<Void> start(String uri, JCredentials credentials) {
        JStandardEventBus<ProxyEvent> proxyEventBus = new JStandardEventBus<>(ProxyEvent.class);
        proxyEventBus.subscribe(
            asList(ProxyEvent.class),
            proxyEvent -> out.println(proxyEvent));
        JControllerEventBus eventBus = new JControllerEventBus();
        eventBus.<OrderEvent>subscribe(
            asList(OrderEvent.OrderStarted$.class, OrderEvent.OrderFinished$.class),
            (stampedEvent, controllerState) -> out.println(orderEventToString(stampedEvent))
        );

        return JControllerProxy
            .start(uri, credentials, JHttpsConfig.empty(), proxyEventBus, eventBus, ConfigFactory.empty())
            .thenAccept(proxy -> {
                try (TestJControllerProxy test = new TestJControllerProxy(proxy)) {
                    test.run();
                }
            });
    }
}
