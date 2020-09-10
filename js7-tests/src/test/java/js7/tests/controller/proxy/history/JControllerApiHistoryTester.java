package js7.tests.controller.proxy.history;

import com.google.common.collect.ImmutableMap;
import java.util.List;
import java.util.Optional;
import java.util.OptionalLong;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;
import js7.base.web.Uri;
import js7.data.event.Event;
import js7.data.event.EventId;
import js7.data.item.VersionId;
import js7.data.order.OrderEvent.OrderFinished$;
import js7.data.order.OrderId;
import js7.data.workflow.WorkflowPath;
import js7.proxy.data.ProxyEvent;
import js7.proxy.javaapi.JControllerApi;
import js7.proxy.javaapi.data.controller.JControllerState;
import js7.proxy.javaapi.data.controller.JEventAndControllerState;
import js7.proxy.javaapi.data.order.JFreshOrder;
import js7.proxy.javaapi.data.workflow.JWorkflowId;
import js7.proxy.javaapi.eventbus.JStandardEventBus;
import reactor.core.Disposable;
import static java.util.concurrent.TimeUnit.SECONDS;
import static js7.proxy.javaapi.data.common.VavrUtils.getOrThrow;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.MatcherAssert.assertThat;

final class JControllerApiHistoryTester
{
    static JWorkflowId TestWorkflowId = JWorkflowId.of(WorkflowPath.of("/WORKFLOW"), VersionId.of("INITIAL"));
    static final OrderId TestOrderId = OrderId.of("ORDER");

    private final JControllerApi api;
    private final WorkflowPath workflowPath;
    private final List<Uri> agentUris;

    JControllerApiHistoryTester(JControllerApi api, WorkflowPath workflowPath, List<Uri> agentUris) {
        this.api = api;
        this.workflowPath = workflowPath;
        this.agentUris = agentUris;
    }

    void test() throws Exception {
        try (JStandardEventBus<ProxyEvent> proxyEventBus = new JStandardEventBus<>(ProxyEvent.class)) {
            InMemoryHistory history = new InMemoryHistory();
            JFreshOrder freshOrder = JFreshOrder.of(TestOrderId, workflowPath, Optional.empty(), ImmutableMap.of("KEY", "VALUE"));
            CompletableFuture<JEventAndControllerState<Event>> whenOrderFinished = new CompletableFuture<>();

            Disposable firstSubscription = api.eventFlux(proxyEventBus, OptionalLong.of(EventId.BeforeFirst()))
                .doOnNext(eventAndState -> {
                    // OrderFinished terminates this test
                    if(eventAndState.stampedEvent().value().event() instanceof OrderFinished$ &&
                        eventAndState.stampedEvent().value().key().equals(freshOrder.id())) {
                        whenOrderFinished.complete(eventAndState);
                    }
                })
                .subscribe(eventAndState -> {
                    history.handleEventAndState(eventAndState);
                    try {
                        // Do this only occassionally, may be one in a quarter hour, to avoid too much events and load:
                        getOrThrow(
                            api.releaseEvents(eventAndState.stampedEvent().eventId()).get(99, SECONDS));
                    } catch (InterruptedException | ExecutionException | TimeoutException e) {
                        throw new RuntimeException(e);
                    }
                });
            try {
                getOrThrow(api.addOrder(freshOrder).get(99, SECONDS));
                whenOrderFinished.get(99, SECONDS);
            } finally {
                firstSubscription.dispose();
            }
            assertThat(history.idToOrderEntry(), equalTo(InMemoryHistory.expectedIdToOrderEntry(agentUris)));

            // A second call with same EventId returns equal JEventAndControllerState
            JEventAndControllerState<Event> expectedES = whenOrderFinished.get(0, SECONDS);
            JControllerState state = api
                .eventFlux(proxyEventBus, OptionalLong.of(expectedES.stampedEvent().eventId()))
                .map(JEventAndControllerState::state)
                .take(1)
                .collectList()
                .toFuture()
                .get(99, SECONDS)
                .get(0);
            assertThat(state, equalTo(expectedES.state()));
        }
    }
}
