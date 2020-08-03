package js7.tests.controller.proxy;

import io.vavr.control.Either;
import java.time.Instant;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import js7.base.crypt.SignedString;
import js7.base.problem.Problem;
import js7.base.problem.ProblemCode;
import js7.data.event.KeyedEvent;
import js7.data.event.Stamped;
import js7.data.filebased.VersionId;
import js7.data.order.OrderEvent;
import js7.data.order.OrderEvent.OrderFinished$;
import js7.data.order.OrderEvent.OrderMoved;
import js7.data.order.OrderEvent.OrderStarted$;
import js7.data.order.OrderId;
import js7.data.workflow.WorkflowPath;
import js7.proxy.ProxyEvent;
import js7.proxy.javaapi.JAdmission;
import js7.proxy.javaapi.JControllerProxy;
import js7.proxy.javaapi.JProxyContext;
import js7.proxy.javaapi.JStandardEventBus;
import js7.proxy.javaapi.data.JControllerState;
import js7.proxy.javaapi.data.JFreshOrder;
import js7.proxy.javaapi.data.JHttpsConfig;
import js7.proxy.javaapi.data.JUpdateRepoOperation;
import js7.proxy.javaapi.data.JWorkflow;
import js7.proxy.javaapi.data.JWorkflowId;
import reactor.core.publisher.Flux;
import static com.google.common.collect.Maps.newHashMap;
import static java.util.Arrays.asList;
import static java.util.concurrent.TimeUnit.SECONDS;
import static js7.proxy.javaapi.data.JKeyedEvent.keyedEventToJson;
import static js7.proxy.javaapi.utils.VavrUtils.getOrThrow;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.MatcherAssert.assertThat;

/**
 * @author Joacim Zschimmer
 */
final class JControllerProxyTester
{
    private static final List<OrderId> orderIds = IntStream.rangeClosed(0, 2)
        .mapToObj(i -> OrderId.of("TEST-ORDER-" + i))
        .collect(Collectors.toList());
    private final Set<OrderId> finishedOrders = new HashSet<>();
    private final String workflowJson;
    private final String agentRefJson;
    private final JControllerProxy proxy;
    private final CouplingState couplingState;
    private final List<KeyedEvent<OrderEvent>> events = new ArrayList<>();
    private final CompletableFuture<Void> finished = new CompletableFuture<>();

    private JControllerProxyTester(String workflowJson, String agentRefJson, JControllerProxy proxy, CouplingState couplingState) {
        this.workflowJson = workflowJson;
        this.agentRefJson = agentRefJson;
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

    private CompletableFuture<Void> whenDecoupled() {
        return couplingState.decoupled;
    }

    private void test() throws Exception {
        couplingState.coupled.get(99, SECONDS);

        String overview = getOrThrow(
            proxy.api().httpGetJson("/controller/api")
                .get(99, SECONDS));
        assertThat(overview.contains("\"id\":\"Controller\""), equalTo(true));

        addItems();
        testRunOrders();
        testCancelOrderViaHttpPost();
    }

    private void addItems() throws Exception {
        VersionId versionId = VersionId.of("MY-VERSION");  // Must match the versionId in added or replaced objects

        JWorkflowId workflowId = JWorkflowId.of(WorkflowPath.of("/WORKFLOW"), versionId);
        assertThat(proxy.currentState().idToWorkflow(workflowId).mapLeft(Problem::codeOrNull),
            equalTo(Either.left(ProblemCode.of("UnknownKey"/*may change*/))));

        getOrThrow(proxy.api()
            .updateRepo(
                versionId,
                Flux.fromIterable(asList(
                    JUpdateRepoOperation.addOrReplace(sign(workflowJson)),
                    JUpdateRepoOperation.addOrReplace(sign(agentRefJson))
                )))
            .get(99, SECONDS));

        // TODO Delete workflows

        JWorkflow workflow = null;
        while (workflow == null) {  // TODO Auf Event warten
            Thread.sleep(100);
            workflow = proxy.currentState().idToWorkflow(workflowId).getOrNull();

        }
        assertThat(workflow.id(), equalTo(workflowId));
    }

    private static SignedString sign(String json) {
        return SignedString.of(
            json,                       // The string to be be signed
            "Silly",                    // Thy signature type, "PGP" (or "Silly" for silly testing)
            "MY-SILLY-SIGNATURE");      // The signature of string
    }

    private void testRunOrders() throws Exception {
        // FIVE WAYS TO ADD AN ORDER (first way is recommended)

        // #1 addOrders(Flux)
        // Idempotent: already existent order (with same OrderId) are silently ignored
        getOrThrow(proxy.api()
            .addOrders(Flux.fromIterable(asList(newOrder(0), newOrder(1))))
            .get(99, SECONDS));


        // #2 addOrder
        Boolean addOrderResponse = getOrThrow(proxy.api()
            .addOrder(newOrder(2))
            .get(99, SECONDS));
        assertThat(addOrderResponse, equalTo(true/*added, no duplicate*/));

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

    private static JFreshOrder newOrder(int index) {
        return JFreshOrder.of(orderIds.get(index), WorkflowPath.of("/WORKFLOW"));
    }

    private void testCancelOrderViaHttpPost() throws Exception {
        boolean added = getOrThrow(proxy.api()
            .addOrder(JFreshOrder.of(
                OrderId.of("TEST-CANCEL"),
                WorkflowPath.of("/WORKFLOW"),
                Optional.of(Instant.parse("2100-01-01T00:00:00Z")),
                newHashMap()))
            .get(99, SECONDS));
        assertThat(added, equalTo(true));

        String response = getOrThrow(proxy.api()
            .httpPostJson("/controller/api/command", "{'TYPE': 'CancelOrder', 'orderId': 'TEST-CANCEL'}"
                .replace('\'', '"'))
            .get(99, SECONDS));
        assertThat(response, equalTo("{\"TYPE\":\"Accepted\"}"));
    }

    static void run(List<JAdmission> admissions, JHttpsConfig httpsConfig,
        String workflowJson, String agentRefJson,
        Runnable startController) throws Exception
    {
        JStandardEventBus<ProxyEvent> proxyEventBus = new JStandardEventBus<>(ProxyEvent.class);

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
                    JControllerProxyTester tester = new JControllerProxyTester(workflowJson, agentRefJson, proxy, couplingState);
                    tester.test();
                    //??? tester.whenDecoupled().get(99, SECONDS);
                } finally {
                    proxy.stop().get(99, SECONDS);
                }
            }
        }
    }
}
