package js7.tests.controller.proxy;

import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;
import js7.base.problem.Problem;
import js7.proxy.ProxyEvent;
import js7.proxy.javaapi.JAdmission;
import js7.proxy.javaapi.JControllerProxy;
import js7.proxy.javaapi.JProxyContext;
import js7.proxy.javaapi.data.JHttpsConfig;
import js7.proxy.javaapi.eventbus.JStandardEventBus;
import static java.util.concurrent.TimeUnit.SECONDS;
import static js7.proxy.javaapi.utils.VavrUtils.getOrThrow;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.MatcherAssert.assertThat;

/**
 * @author Joacim Zschimmer
 */
final class JControllerProxyTester
{
    private final JControllerProxy proxy;

    private JControllerProxyTester(JControllerProxy proxy) {
        this.proxy = proxy;
    }

    private void test(List<String> itemJsons) throws Exception {
        testHttpGet();

        JControllerProxyRepoTester repoTester = new JControllerProxyRepoTester(proxy);
        repoTester.addItems(itemJsons);
        repoTester.deleteItem();

        try (JControllerProxyOrderTester orderTester = new JControllerProxyOrderTester(proxy)) {
            orderTester.testRunOrders();
            orderTester.testCancelOrderViaHttpPost();
        }
    }

    private void testHttpGet() throws InterruptedException, ExecutionException, TimeoutException {
        String overview = getOrThrow(
            proxy.api().httpGetJson("/controller/api")
                .get(99, SECONDS));
        assertThat(overview.contains("\"id\":\"Controller\""), equalTo(true));
    }

    static void run(List<JAdmission> admissions, JHttpsConfig httpsConfig, List<String> itemJsons, Runnable startController)
        throws Exception
    {
        try (JStandardEventBus<ProxyEvent> proxyEventBus = new JStandardEventBus<>(ProxyEvent.class)) {
            try (CouplingState couplingState = new CouplingState(proxyEventBus)) {
                try (JProxyContext context = new JProxyContext()) {
                    CompletableFuture<JControllerProxy> whenStarted = context
                        .newControllerApi(admissions, httpsConfig)
                        .startProxy(proxyEventBus);

                    // Avoid deadlock while blocking for firstProblem but whenStarted failed
                    Object maybeProblem = CompletableFuture.anyOf(couplingState.firstProblem, whenStarted).get(99, SECONDS);
                    if (maybeProblem instanceof Problem) {
                        Problem problem = (Problem)maybeProblem;
                        assertThat(problem.toString().contains("java.net.ConnectException: Connection refused"), equalTo(true));
                    }

                    startController.run();
                    JControllerProxy proxy = whenStarted.get(99, SECONDS);
                    try {
                        couplingState.coupled.get(99, SECONDS);
                        JControllerProxyTester tester = new JControllerProxyTester(proxy);
                        tester.test(itemJsons);
                    } finally {
                        proxy.stop().get(99, SECONDS);
                    }
                }
            }
        }
    }
}
