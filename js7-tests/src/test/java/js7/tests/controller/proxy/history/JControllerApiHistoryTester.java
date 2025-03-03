package js7.tests.controller.proxy.history;

import java.util.List;
import java.util.Optional;
import java.util.OptionalLong;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import js7.base.problem.ProblemCode;
import js7.base.problem.ProblemException;
import js7.base.web.Uri;
import js7.data.event.Event;
import js7.data.event.EventId;
import js7.data.item.VersionId;
import js7.data.order.OrderEvent;
import js7.data.order.OrderId;
import js7.data.plan.PlanId;
import js7.data.value.StringValue;
import js7.data.workflow.WorkflowPath;
import js7.data_for_java.controller.JControllerState;
import js7.data_for_java.order.JFreshOrder;
import js7.data_for_java.order.JOrder;
import js7.data_for_java.order.JOrderEvent;
import js7.data_for_java.problem.JProblem;
import js7.data_for_java.workflow.JWorkflowId;
import js7.proxy.data.event.ProxyEvent;
import js7.proxy.javaapi.JControllerApi;
import js7.proxy.javaapi.data.controller.JEventAndControllerState;
import js7.proxy.javaapi.eventbus.JStandardEventBus;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import reactor.core.publisher.Mono;
import static java.util.Collections.singletonMap;
import static java.util.concurrent.TimeUnit.SECONDS;
import static js7.data_for_java.vavr.VavrUtils.await;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.MatcherAssert.assertThat;

final class JControllerApiHistoryTester
{
    private static final Logger logger = LoggerFactory.getLogger(JControllerApiHistoryTester.class);
    static JWorkflowId TestWorkflowId = JWorkflowId.of(WorkflowPath.of("WORKFLOW"), VersionId.of("INITIAL"));
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
            CompletableFuture<Optional<JControllerState>> whenFirstFluxTerminated = api
                .eventFlux(proxyEventBus, OptionalLong.of(EventId.BeforeFirst()))
                .doOnNext(eventAndState -> {
                    logger.debug("doOnNext: " + eventAndState.stampedEvent());
                    history.update(eventAndState);
                })
                .doOnNext(es -> {
                    if (false/*periodically after 15 minutes, for example*/) {
                        await(api.releaseEvents(history.eventId()));
                    }})
                .takeUntil(x -> Optional.ofNullable(history.idToOrderEntry().get(TestOrderId))
                    .map(o -> o.terminatedAt().isPresent())
                    .equals(Optional.of(true)))
                .map(es -> Optional.of(es.state()))
                .last(Optional.empty())
                .toFuture();
            JControllerState state;
            try {
                JFreshOrder freshOrder = JFreshOrder.of(TestOrderId, workflowPath,
                    singletonMap("KEY", StringValue.of("VALUE")),
                    PlanId.Global, Optional.empty());
                await(api.addOrder(freshOrder));
                state = whenFirstFluxTerminated.get(99, SECONDS).get();
                assertThat(
                    state.idToOrder().get(freshOrder.id()).checkedState(JOrder.finished()).isRight(),
                    equalTo(true));
            } finally {
                whenFirstFluxTerminated.cancel(false);
            }
            // May throw CancellationException or some other exception due to cancel():
            whenFirstFluxTerminated
                .exceptionally(t -> {
                    logger.debug("Event stream terminated with error after cancel: " + t);
                    return Optional.empty();
                })
                .get(99, SECONDS);
            assertThat(history.idToOrderEntry(), equalTo(InMemoryHistory.expectedIdToOrderEntry(agentUris)));

            // A second call with same EventId returns equal JEventAndControllerState
            JControllerState second = api
                .eventFlux(proxyEventBus, OptionalLong.of(state.eventId()))
                .map(JEventAndControllerState::state)
                .take(1)
                .collectList()
                .toFuture()
                .get(99, SECONDS)
                .get(0);
            assertThat(second, equalTo(state));
        }
    }

    void testTorn() throws Exception {
        await(api.takeSnapshot());
        await(api.releaseEvents(await(api.journalInfo()).lastEventId()));
        // ReleaseEvents löscht Journaldateien sofort wegen wegen js7.journal.release-events-delay = 0s in diesem Test
        final OrderId orderId = OrderId.of("ORDER-TORN");

        try (JStandardEventBus<ProxyEvent> proxyEventBus = new JStandardEventBus<>(ProxyEvent.class)) {
            try {
                api.eventFlux(proxyEventBus, OptionalLong.of(EventId.BeforeFirst()))
                    .then(Mono.<JProblem>error(new RuntimeException("Unexpected end of event steam")))
                    .toFuture()
                    .get(99, SECONDS);
            } catch(ExecutionException e) {
                if (!(e.getCause() instanceof ProblemException) ||
                    !ProblemCode.of("SnapshotForUnknownEventId").equals(((ProblemException)e.getCause()).problem().codeOrNull()))
                    throw e;
            }
            long tornEventId = await(api.journalInfo()).tornEventId();

            CompletableFuture<JEventAndControllerState<Event>> whenFirstFluxTerminated =
                api.eventFlux(proxyEventBus, OptionalLong.of(tornEventId))
                    .doOnNext(eventAndState -> logger.debug("doOnNext: " + eventAndState.stampedEvent()))
                    .takeUntil(es ->
                        es.stampedEvent().value().key().equals(orderId) &&
                        es.stampedEvent().value().event() instanceof OrderEvent.OrderFinished)
                    .last()
                    .toFuture();

            JFreshOrder freshOrder = JFreshOrder.of(orderId, workflowPath,
                singletonMap("KEY", StringValue.of("VALUE")),
                PlanId.Global, Optional.empty());
            await(api.addOrder(freshOrder));
            Event lastEvent = whenFirstFluxTerminated.get(99, SECONDS).stampedEvent().value().event();
            assertThat(
                JOrderEvent.of((OrderEvent)lastEvent),  instanceOf(JOrderEvent.JOrderFinished.class));
        }
    }
}
