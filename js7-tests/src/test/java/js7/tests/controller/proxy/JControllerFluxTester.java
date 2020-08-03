package js7.tests.controller.proxy;

import java.util.concurrent.CompletableFuture;
import js7.data.event.Event;
import js7.data.order.OrderEvent.OrderFinished$;
import js7.data.order.OrderId;
import js7.data.workflow.WorkflowPath;
import js7.proxy.javaapi.JAdmission;
import js7.proxy.javaapi.JControllerApi;
import js7.proxy.javaapi.JControllerProxy;
import js7.proxy.javaapi.JEventAndControllerState;
import js7.proxy.javaapi.JProxyContext;
import js7.proxy.javaapi.data.JFreshOrder;
import js7.proxy.javaapi.data.JHttpsConfig;
import reactor.core.Disposable;
import reactor.core.publisher.Flux;
import static java.util.concurrent.TimeUnit.SECONDS;
import static js7.proxy.javaapi.utils.VavrUtils.getOrThrow;

/** Java test using Reactor streaming.
 * @author Joacim Zschimmer
 */
public final class JControllerFluxTester
implements AutoCloseable
{
    private final JProxyContext context = new JProxyContext();
    private final JControllerApi api;
    private final CompletableFuture<JControllerProxy> whenProxy;
    private int orderCounter = 0;

    public JControllerFluxTester(Iterable<JAdmission> admissions, JHttpsConfig httpsConfig) {
        api = context.newControllerApi(admissions, httpsConfig);
        whenProxy = api.startProxy();
    }

    public void close() {
        context.close();
    }

    void test() throws Exception {
        test1();
        test1();  // it is repeatable
    }

    private void test1() throws Exception {
        JControllerProxy proxy = whenProxy.get(99, SECONDS);
        orderCounter++;
        OrderId orderId = OrderId.of("ORDER-" + orderCounter);

        CompletableFuture<Void> orderFinished = new CompletableFuture<>();
        Flux<JEventAndControllerState<Event>> flux = proxy.flux()
            .doOnNext(eventAndState -> {
                Event event = eventAndState.stampedEvent().value().event();
                if (event instanceof OrderFinished$ && eventAndState.stampedEvent().value().key().equals(orderId)) {
                    orderFinished.complete(null);
                }
            });
        Disposable subscription = flux.subscribe();
        try {
            getOrThrow(api
                .addOrder(JFreshOrder.of(orderId, WorkflowPath.of("/WORKFLOW")))
                .get(99, SECONDS));
            orderFinished.get(99, SECONDS);
        } finally {
            subscription.dispose();
        }
    }
}
