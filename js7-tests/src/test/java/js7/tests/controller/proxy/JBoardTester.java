package js7.tests.controller.proxy;

import java.time.Instant;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;
import java.util.function.Predicate;
import js7.base.crypt.SignedString;
import js7.data.board.NoticeId;
import js7.data.event.Event;
import js7.data.event.KeyedEvent;
import js7.data.item.InventoryItemPath;
import js7.data.item.UnsignedSimpleItemEvent.UnsignedSimpleItemAdded;
import js7.data.item.VersionId;
import js7.data.order.OrderEvent;
import js7.data.order.OrderId;
import js7.data.workflow.WorkflowPath;
import js7.data_for_java.board.JBoard;
import js7.data_for_java.board.JNotice;
import js7.data_for_java.board.JNoticeExpectation;
import js7.data_for_java.board.JNoticePlace;
import js7.data_for_java.item.JUpdateItemOperation;
import js7.data_for_java.order.JFreshOrder;
import js7.data_for_java.value.JExpression;
import js7.proxy.javaapi.JControllerApi;
import js7.proxy.javaapi.JControllerProxy;
import js7.proxy.javaapi.data.controller.JEventAndControllerState;
import org.hamcrest.Matchers;
import reactor.core.publisher.Flux;
import static io.vavr.control.Either.right;
import static java.util.Arrays.asList;
import static java.util.concurrent.TimeUnit.SECONDS;
import static js7.data_for_java.item.JUpdateItemOperation.addVersion;
import static js7.data_for_java.vavr.VavrUtils.await;
import static js7.data_for_java.vavr.VavrUtils.getOrThrow;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.MatcherAssert.assertThat;

public class JBoardTester
{
    private static final VersionId versionId = JControllerProxyTest.boardVersion();
    private static final WorkflowPath postingBoardWorkflowPath =
        (WorkflowPath)JControllerProxyTest.postingBoardWorkflow().path();
    private static final WorkflowPath expectingBoardWorkflowPath =
        (WorkflowPath)JControllerProxyTest.expectingBoardWorkflow().path();
    private static final JBoard board = JBoard.of(JControllerProxyTest.boardPath(),
            getOrThrow(JExpression.parse(
                "replaceAll($js7OrderId, '^#([0-9]{4}-[0-9]{2}-[0-9]{2})#.*$', \"\\$1\")")),
            getOrThrow(JExpression.parse(
                "replaceAll($js7OrderId, '^#([0-9]{4}-[0-9]{2}-[0-9]{2})#.*$', \"\\$1\")")),
            getOrThrow(JExpression.parse(
                "$epochMilli + 24 * 3600 * 1000")));

    private final JControllerProxy proxy;
    private final JControllerApi api;

    JBoardTester(JControllerProxy proxy) {
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
                .pathToBoard(board.path())
                .map(o -> o.withRevision(Optional.empty())),
            equalTo(right(board)));
    }

    private void testPostedNotice() throws ExecutionException, InterruptedException, TimeoutException {
        NoticeId postedNoticeId = NoticeId.of("2021-01-01");
        OrderId posterOrderId = OrderId.of("#2021-01-01#POSTER");
        CompletableFuture<JEventAndControllerState<Event>> whenPosted =
            awaitEvent(keyedEvent ->
                keyedEvent.key().equals(posterOrderId)
                    && keyedEvent.event() instanceof OrderEvent.OrderNoticePosted);
        await(
            api.addOrders(Flux.just(JFreshOrder.of(posterOrderId, postingBoardWorkflowPath))));

        whenPosted.get(99, SECONDS);
        JNoticePlace postedNotice =
            getOrThrow(proxy
                .currentState()
                .pathToBoardState(board.path()))
            .idToNotice(postedNoticeId)
            .get();
        assert postedNotice instanceof JNotice;
        assertThat(postedNotice.id(), equalTo(postedNoticeId));
        assertThat(((JNotice)postedNotice).endOfLife(), Matchers.greaterThan(Instant.now()));
    }

    private void testExpectedNotice() throws ExecutionException, InterruptedException, TimeoutException {
        NoticeId expectedNoticeId = NoticeId.of("2021-07-20");
        OrderId expectingOrderId = OrderId.of("#2021-07-20#READER");

        CompletableFuture<JEventAndControllerState<Event>> whenWaiting =
            awaitEvent(keyedEvent ->
                keyedEvent.key().equals(expectingOrderId)
                    && keyedEvent.event() instanceof OrderEvent.OrderNoticeExpected);
        await(
            api.addOrders(Flux.just(JFreshOrder.of(expectingOrderId, expectingBoardWorkflowPath))));

        whenWaiting.get(99, SECONDS);
        JNoticePlace noticeExpectation =
            getOrThrow(proxy
                .currentState()
                .pathToBoardState(board.path()))
            .idToNotice(expectedNoticeId)
            .get();
        assert noticeExpectation instanceof JNoticeExpectation;
        assertThat(noticeExpectation.id(), equalTo(expectedNoticeId));
        assertThat(((JNoticeExpectation)noticeExpectation).orderIds(),
            equalTo(asList(expectingOrderId)));
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
