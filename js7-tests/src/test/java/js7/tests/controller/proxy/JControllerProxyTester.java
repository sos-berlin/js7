package js7.tests.controller.proxy;

import java.util.List;
import java.util.concurrent.CompletableFuture;
import js7.base.problem.Problem;
import js7.data_for_java.auth.JAdmission;
import js7.data_for_java.auth.JHttpsConfig;
import js7.proxy.data.event.ProxyEvent;
import js7.proxy.javaapi.JControllerApi;
import js7.proxy.javaapi.JControllerProxy;
import js7.proxy.javaapi.JProxyContext;
import js7.proxy.javaapi.eventbus.JStandardEventBus;
import static java.util.concurrent.TimeUnit.SECONDS;
import static js7.data_for_java.vavr.VavrUtils.await;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.MatcherAssert.assertThat;

/**
 * @author Joacim Zschimmer
 */
class JControllerProxyTester
{
    private final JControllerProxy proxy;
    private final JControllerApi api;

    private JControllerProxyTester(JControllerProxy proxy) {
        this.proxy = proxy;
        this.api = proxy.api();
    }

    private void test(List<String> itemJsons, List<String> manyItemJsons) throws Exception {
        testHttpGet();

        JControllerProxyRepoTester repoTester = new JControllerProxyRepoTester(proxy);
        repoTester.addTamperedItems(manyItemJsons);
        repoTester.addItems(itemJsons);
        repoTester.removeWorkflow();

        new JJobResourceTester(proxy).run();

        JControllerApiOrderTester apiOrderTester = new JControllerApiOrderTester(api);
        apiOrderTester.testCancelOrder();

        try (JControllerProxyEventBusOrderTester orderTester = new JControllerProxyEventBusOrderTester(proxy)) {
            orderTester.testRunOrders();
        }
        try (JControllerProxyAddOrderIdempotentlyTester orderTester = new JControllerProxyAddOrderIdempotentlyTester(proxy)) {
            orderTester.testRunOrders();
        }
    }

    private void testHttpGet() {
        String overview = await(api.httpGetJson("/controller/api"));
        assertThat(overview.contains("\"id\":\"Controller\""), equalTo(true));
    }

    static void run(List<JAdmission> admissions, JHttpsConfig httpsConfig,
        List<String> itemJsons, List<String> manyItemJsons, Runnable startController)
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
                        new JControllerProxyTester(proxy).test(itemJsons, manyItemJsons);
                        new JBoardTester(proxy).test();
                    } finally {
                        proxy.stop().get(99, SECONDS);
                    }
                }
            }
        }
    }
}
