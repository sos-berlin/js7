package js7.tests.controller.proxy;

import com.typesafe.config.ConfigFactory;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ForkJoinPool;
import java.util.concurrent.TimeoutException;
import js7.data.event.Event;
import js7.data.order.OrderEvent.OrderFinished;
import js7.data.order.OrderId;
import js7.data.workflow.WorkflowPath;
import js7.data_for_java.auth.JAdmission;
import js7.data_for_java.auth.JHttpsConfig;
import js7.data_for_java.order.JFreshOrder;
import js7.proxy.javaapi.JControllerApi;
import js7.proxy.javaapi.JControllerProxy;
import js7.proxy.javaapi.JProxyContext;
import js7.proxy.javaapi.data.controller.JEventAndControllerState;
import reactor.core.Disposable;
import reactor.core.publisher.Flux;
import static java.util.concurrent.TimeUnit.SECONDS;
import static js7.data_for_java.vavr.VavrUtils.await;

/** Java test using Reactor streaming.
 * @author Joacim Zschimmer
 */
public final class JControllerFluxTester
implements AutoCloseable
{
    private final JProxyContext context = new JProxyContext(ConfigFactory.empty(), ForkJoinPool.commonPool());
    private final JControllerApi api;
    private final JControllerProxy proxy;
    private int orderCounter = 0;

    public JControllerFluxTester(Iterable<JAdmission> admissions, JHttpsConfig httpsConfig)
        throws InterruptedException, ExecutionException, TimeoutException {
        api = context.newControllerApi(admissions, httpsConfig);
        proxy = api.startProxy().get(99, SECONDS);
    }

    public void close() {
        context.close();
    }

    void test() throws Exception {
        test1();
        test1();  // it is repeatable
    }

    private void test1() throws Exception {
        orderCounter++;
        OrderId orderId = OrderId.of("ORDER-" + orderCounter);

        CompletableFuture<Void> orderFinished = new CompletableFuture<>();
        Flux<JEventAndControllerState<Event>> flux = proxy.flux()
            .doOnNext(eventAndState -> {
                if (eventAndState.stampedEvent().value().event() instanceof OrderFinished &&
                    eventAndState.stampedEvent().value().key().equals(orderId)) {
                    orderFinished.complete(null);
                }
            });
        Disposable subscription = flux.subscribe();
        try {
            await(api.addOrder(JFreshOrder.of(orderId, WorkflowPath.of("WORKFLOW"))));
            orderFinished.get(99, SECONDS);
        } finally {
            subscription.dispose();
        }
    }
}
