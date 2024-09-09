package js7.tests.controller.proxy;

import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.CompletableFuture;
import js7.base.problem.Problem;
import js7.base.web.Uri;
import js7.data.agent.AgentPath;
import js7.data.event.Event;
import js7.data.event.KeyedEvent;
import js7.data.item.BasicItemEvent.ItemAttachable;
import js7.data.item.BasicItemEvent.ItemAttached;
import js7.data.item.BasicItemEvent.ItemDeleted;
import js7.data.item.BasicItemEvent.ItemDeletionMarked;
import js7.data.item.BasicItemEvent.ItemDetachable;
import js7.data.item.BasicItemEvent.ItemDetached;
import js7.data.item.InventoryItemEvent;
import js7.data.item.InventoryItemKey;
import js7.data.item.ItemRevision;
import js7.data.item.UnsignedSimpleItemEvent.UnsignedSimpleItemAdded;
import js7.data.subagent.SubagentId;
import js7.data.subagent.SubagentSelectionId;
import js7.data_for_java.auth.JAdmission;
import js7.data_for_java.auth.JHttpsConfig;
import js7.data_for_java.subagent.JSubagentItem;
import js7.data_for_java.subagent.JSubagentSelection;
import js7.data_for_java.value.JExpression;
import js7.proxy.data.event.ProxyEvent;
import js7.proxy.javaapi.JControllerApi;
import js7.proxy.javaapi.JControllerProxy;
import js7.proxy.javaapi.JProxyContext;
import js7.proxy.javaapi.data.controller.JEventAndControllerState;
import js7.proxy.javaapi.eventbus.JStandardEventBus;
import reactor.core.Disposable;
import reactor.core.publisher.Flux;
import scala.Some;
import static java.util.Arrays.asList;
import static java.util.concurrent.TimeUnit.SECONDS;
import static js7.data_for_java.item.JUpdateItemOperation.addOrChangeSimple;
import static js7.data_for_java.item.JUpdateItemOperation.deleteSimple;
import static js7.data_for_java.vavr.VavrUtils.await;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.MatcherAssert.assertThat;

final class JSubagentTester
{
    private static final AgentPath agentPath = AgentPath.of("AGENT");

    private static final JSubagentItem subagentItem = JSubagentItem.of(
        SubagentId.of("SUBAGENT"),
        agentPath,
        Uri.of("http://localhost:0"),
        /*disabled=*/false);

    private static final JSubagentSelection subagentSelection = JSubagentSelection.of(
        SubagentSelectionId.of("SUBAGENT"),
        new HashMap() {{
            put(subagentItem.id(), /*priority=*/JExpression.apply("1"));
        }});

    private static final Set<InventoryItemKey> keys = new HashSet<>(asList(
        subagentItem.id(), subagentSelection.id()));

    private final JControllerProxy proxy;
    private final JControllerApi api;

    private JSubagentTester(JControllerProxy proxy) {
        this.proxy = proxy;
        this.api = proxy.api();
    }

    private void test() throws Exception {
        CompletableFuture<Void> completed = new CompletableFuture<>();
        Set<KeyedEvent<Event>> keyedEvents = new HashSet<>();

        Flux<JEventAndControllerState<Event>> flux = proxy.flux()
            .doOnNext(eventAndState -> {
                KeyedEvent<Event> keyedEvent = eventAndState.stampedEvent().value();
                if (keyedEvent.event() instanceof InventoryItemEvent) {
                    InventoryItemEvent event = (InventoryItemEvent)keyedEvent.event();
                    if (keys.contains(event.key())) {
                       keyedEvents.add(keyedEvent);
                    }
                }
                if (keyedEvent.equals(KeyedEvent.of(new ItemDeleted(subagentItem.id())))) {
                    completed.complete((Void)null);
                }
            });
        Disposable subscription = flux.subscribe();

        await(api.updateItems(Flux.just(
            addOrChangeSimple(subagentItem),
            addOrChangeSimple(subagentSelection))));

        try {
            await(api.updateItems(Flux.just(
                deleteSimple(subagentSelection.id()))));
            await(api.updateItems(Flux.just(
                deleteSimple(subagentItem.id()))));
            completed.get(99, SECONDS);
        } finally {
            subscription.dispose();
        }
        assertThat(keyedEvents, equalTo(new HashSet<KeyedEvent<Event>>(asList(
            KeyedEvent.any(new UnsignedSimpleItemAdded(subagentItem
                .withRevision(Optional.of(new ItemRevision(0))).asScala())),
            KeyedEvent.any(new UnsignedSimpleItemAdded(subagentSelection
                .withRevision(Optional.of(new ItemRevision(0))).asScala())),
            KeyedEvent.any(new ItemAttachable(subagentItem.id(), agentPath)),
            KeyedEvent.any(new ItemAttached(subagentItem.id(), new Some<>(new ItemRevision(0)), agentPath)),
            KeyedEvent.any(new ItemDetachable(subagentItem.id(), agentPath)),
            KeyedEvent.any(new ItemDetached(subagentItem.id(), agentPath)),
            KeyedEvent.any(new ItemDeletionMarked(subagentItem.id())),
            KeyedEvent.any(new ItemDeleted(subagentItem.id())),
            KeyedEvent.any(new ItemDeleted(subagentSelection.id()))
        ))));
    }

    static void run(List<JAdmission> admissions, JHttpsConfig httpsConfig) throws Exception {
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

                    JControllerProxy proxy = whenStarted.get(99, SECONDS);
                    try {
                        couplingState.coupled.get(99, SECONDS);
                        new JSubagentTester(proxy).test();
                    } finally {
                        proxy.stop().get(99, SECONDS);
                    }
                }
            }
        }
    }
}
