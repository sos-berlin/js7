package js7.tests.controller.proxy;

import java.time.Instant;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;
import java.util.stream.Collectors;
import js7.data.event.KeyedEvent;
import js7.data.event.Stamped;
import js7.data.order.OrderEvent;
import js7.data.order.OrderId;
import js7.proxy.ProxyEvent;
import js7.proxy.javaapi.JControllerProxy;
import js7.proxy.javaapi.JCredentials;
import js7.proxy.javaapi.JProxyContext;
import js7.proxy.javaapi.JStandardEventBus;
import js7.proxy.javaapi.data.JControllerState;
import js7.proxy.javaapi.data.JHttpsConfig;
import static java.lang.System.out;
import static java.util.Arrays.asList;
import static java.util.concurrent.TimeUnit.SECONDS;

/** Example for usage of JControllerProxy.
 * @author Joacim Zschimmer
 */
public final class TestJControllerProxy
{
    private final JControllerProxy proxy;

    private TestJControllerProxy(JControllerProxy proxy) {
        this.proxy = proxy;
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
    public static void main(String[] args) throws Exception {
        if (args.length != 1 && args.length != 3) throw new IllegalArgumentException("One or three arguments required: URI USERID PASSWORD");
        String uri = args[0];
        final JCredentials credentials = args.length == 3 ? JCredentials.of(args[1], args[2]) : JCredentials.noCredentials();
        run(uri, credentials);
    }

    private static void run(String uri, JCredentials credentials) throws InterruptedException, ExecutionException, TimeoutException {
        try(JProxyContext context = new JProxyContext()) {
            JStandardEventBus<ProxyEvent> proxyEventBus = new JStandardEventBus<>(ProxyEvent.class);
            proxyEventBus.subscribe(
                asList(ProxyEvent.class),
                out::println);

            JControllerProxy proxy = context
                .startControllerProxy(uri, credentials, JHttpsConfig.empty(), proxyEventBus)
                .get(99, SECONDS);
            try {
                proxy.controllerEventBus().<OrderEvent>subscribe(
                    asList(OrderEvent.OrderStarted$.class, OrderEvent.OrderFinished$.class),
                    (stampedEvent, controllerState) -> out.println(orderEventToString(stampedEvent))
                );
                while (true) {
                    out.println(controllerStateToString(proxy.currentState()));
                    sleep(1000);
                }
            } finally {
                proxy.stop().get(99, SECONDS);
            }
        }
    }
}
