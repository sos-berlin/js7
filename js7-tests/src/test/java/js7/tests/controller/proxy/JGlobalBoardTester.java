package js7.tests.controller.proxy;

import java.time.Instant;
import java.util.HashSet;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;
import java.util.function.Predicate;
import js7.base.crypt.SignedString;
import js7.data.board.NoticeId;
import js7.data.board.NoticeKey;
import js7.data.board.PlannedNoticeKey;
import js7.data.event.Event;
import js7.data.event.KeyedEvent;
import js7.data.item.InventoryItemPath;
import js7.data.item.UnsignedSimpleItemEvent.UnsignedSimpleItemAdded;
import js7.data.item.VersionId;
import js7.data.order.OrderEvent;
import js7.data.order.OrderId;
import js7.data.plan.PlanId;
import js7.data.workflow.WorkflowPath;
import js7.data_for_java.board.JGlobalBoard;
import js7.data_for_java.board.JNoticePlace;
import js7.data_for_java.item.JUpdateItemOperation;
import js7.data_for_java.order.JFreshOrder;
import js7.data_for_java.value.JExpression;
import js7.proxy.javaapi.JControllerApi;
import js7.proxy.javaapi.JControllerProxy;
import js7.proxy.javaapi.data.controller.JEventAndControllerState;
import org.hamcrest.Matchers;
import reactor.core.publisher.Flux;
import static java.util.Collections.singleton;
import static java.util.concurrent.TimeUnit.SECONDS;
import static js7.data_for_java.item.JUpdateItemOperation.addVersion;
import static js7.data_for_java.vavr.VavrUtils.await;
import static js7.data_for_java.vavr.VavrUtils.getOrThrow;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.MatcherAssert.assertThat;

public class JGlobalBoardTester
{
    private static final VersionId versionId = JControllerProxyTest.boardVersion();
    private static final WorkflowPath postingBoardWorkflowPath =
        JControllerProxyTest.postingBoardWorkflow().path();
    private static final WorkflowPath expectingBoardWorkflowPath =
        JControllerProxyTest.expectingBoardWorkflow().path();
    private static final JGlobalBoard board = JGlobalBoard.of(
        JControllerProxyTest.boardPath(),
        getOrThrow(JExpression.parse(
            "match($js7OrderId, '#([0-9]{4}-[0-9]{2}-[0-9]{2})#.*', \"\\$1\")")),
        getOrThrow(JExpression.parse(
            "match($js7OrderId, '#([0-9]{4}-[0-9]{2}-[0-9]{2})#.*', \"\\$1\")")),
        getOrThrow(JExpression.parse(
            "$js7EpochMilli + 24 * 3600 * 1000")));

    private final JControllerProxy proxy;
    private final JControllerApi api;

    JGlobalBoardTester(JControllerProxy proxy) {
        this.proxy = proxy;
        this.api = proxy.api();
    }

    void test()
        throws InterruptedException, ExecutionException, TimeoutException
    {
        testBoard();
        testPostedNotice();
        testExpectedNotice();
    }

    private void testBoard() throws ExecutionException, InterruptedException, TimeoutException {
        CompletableFuture<JEventAndControllerState<Event>> whenBoardAdded =
            awaitEvent(keyedEvent -> isItemAdded(keyedEvent, board.path()));

        await(api.updateItems(Flux.concat(
            Flux.just(board)
                .map(JUpdateItemOperation::addOrChangeSimple),
            Flux.just(addVersion(versionId)),
            Flux
                .just(
                    JControllerProxyTest.postingBoardWorkflowJson(),
                    JControllerProxyTest.expectingBoardWorkflowJson())
                .map(o -> sign(o))
                .map(JUpdateItemOperation::addOrChangeSigned))));

        whenBoardAdded.get(99, SECONDS);
        assertThat(proxy
                .currentState()
                .pathToBoard().get(board.path())
                .withRevision(Optional.empty()),
            equalTo(board));
    }

    private void testPostedNotice() throws ExecutionException, InterruptedException, TimeoutException {
        PlannedNoticeKey postedPlannedNoticeKey = PlannedNoticeKey.of("2021-01-01");
        NoticeId postedNoticeId = NoticeId.of(PlanId.Global(), board.path(), NoticeKey.of("2021-01-01"));
        OrderId posterOrderId = OrderId.of("#2021-01-01#POSTER");
        CompletableFuture<JEventAndControllerState<Event>> whenPosted =
            awaitEvent(keyedEvent ->
                keyedEvent.key().equals(posterOrderId)
                    && keyedEvent.event() instanceof OrderEvent.OrderNoticePosted);
        await(
            api.addOrders(Flux.just(JFreshOrder.of(posterOrderId, postingBoardWorkflowPath))));

        whenPosted.get(99, SECONDS);
        JNoticePlace noticePlace = proxy
            .currentState()
            .pathToBoardState().get(board.path())
            .toNotice(postedPlannedNoticeKey)
            .get();
        assert noticePlace.notice().isPresent();
        assertThat(noticePlace.notice().get().endOfLife().get(), Matchers.greaterThan(Instant.now()));
    }

    private void testExpectedNotice() throws ExecutionException, InterruptedException, TimeoutException {
        PlannedNoticeKey expectedPlannedNoticeKey = PlannedNoticeKey.of("2021-07-20");
        NoticeId expectedNoticeId = NoticeId.of(PlanId.Global(), board.path(), NoticeKey.of("2021-07-20"));
        OrderId expectingOrderId = OrderId.of("#2021-07-20#READER");

        CompletableFuture<JEventAndControllerState<Event>> whenWaiting =
            awaitEvent(keyedEvent ->
                keyedEvent.key().equals(expectingOrderId)
                    && keyedEvent.event() instanceof OrderEvent.OrderNoticesExpected);
        await(
            api.addOrders(Flux.just(JFreshOrder.of(expectingOrderId, expectingBoardWorkflowPath))));

        whenWaiting.get(99, SECONDS);
        JNoticePlace noticePlace =
            proxy
                .currentState()
                .pathToBoardState().get(board.path())
            .toNotice(expectedPlannedNoticeKey)
            .get();
        assertThat(noticePlace.expectingOrderIds(),
            equalTo(new HashSet<>(singleton(expectingOrderId))));
    }

    private static boolean isItemAdded(KeyedEvent<Event> keyedEvent, InventoryItemPath path) {
        Event event = keyedEvent.event();
        return event instanceof UnsignedSimpleItemAdded &&
            ((UnsignedSimpleItemAdded)event).key().equals(path);
    }

    private static SignedString sign(String json) {
        return SignedString.of(
            json,                       // The string to be be signed
            "Silly",                    // Thy signature type, "X509" or "PGP" (or "Silly" for silly testing)
            "MY-SILLY-SIGNATURE");      // The signature of string (in case of X.509: MIME base64 encoded)
    }

    private CompletableFuture<JEventAndControllerState<Event>> awaitEvent(Predicate<KeyedEvent<Event>> predicate) {
        return proxy
            .flux()
            .skipWhile(o -> !predicate.test(o.stampedEvent().value()))
            .elementAt(0)
            .toFuture();
    }
}
