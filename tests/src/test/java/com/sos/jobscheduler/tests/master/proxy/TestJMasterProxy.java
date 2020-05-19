package com.sos.jobscheduler.tests.master.proxy;

import com.sos.jobscheduler.data.event.KeyedEvent;
import com.sos.jobscheduler.data.event.Stamped;
import com.sos.jobscheduler.data.order.OrderEvent;
import com.sos.jobscheduler.data.order.OrderId;
import com.sos.jobscheduler.proxy.javaapi.JCredentials;
import com.sos.jobscheduler.proxy.javaapi.JMasterProxy;
import com.sos.jobscheduler.proxy.javaapi.JProxyEventBus;
import com.sos.jobscheduler.proxy.javaapi.data.JMasterState;
import com.typesafe.config.ConfigFactory;
import java.time.Instant;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.stream.Collectors;
import static java.lang.System.out;
import static java.util.Arrays.asList;

/** Example for usage of JMasterProxy.
 * @author Joacim Zschimmer
 */
public final class TestJMasterProxy implements AutoCloseable
{
    private final JMasterProxy proxy;

    private TestJMasterProxy(JMasterProxy proxy) {
        this.proxy = proxy;
    }

    public void close() {
        proxy.close();
    }

    private void run() {
        while (true) {
            out.println(masterStateToString(proxy.currentState()));
            sleep(1000);
        }
    }

    private static String masterStateToString(JMasterState masterState) {
        return //Instant.ofEpochMilli(EventId.toEpochMilli(masterState.eventId())) + " " +
            masterState.orderIds().size() +
            " orders: " +
            masterState.orderIds().stream()
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
        JProxyEventBus eventBus = new JProxyEventBus();
        eventBus.<OrderEvent>subscribe(
            asList(OrderEvent.OrderStarted$.class, OrderEvent.OrderFinished$.class),
            (stampedEvent, masterState) -> out.println(orderEventToString(stampedEvent))
        );

        return JMasterProxy
            .start(uri, credentials, eventBus, ConfigFactory.empty())
            .thenAccept(proxy -> {
                try (TestJMasterProxy test = new TestJMasterProxy(proxy)) {
                    test.run();
                }
            });
    }
}
