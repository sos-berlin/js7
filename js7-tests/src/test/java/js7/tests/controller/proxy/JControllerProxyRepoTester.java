package js7.tests.controller.proxy;

import io.vavr.Tuple2;
import io.vavr.control.Either;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;
import java.util.function.Predicate;
import java.util.stream.Stream;
import js7.base.crypt.SignedString;
import js7.base.problem.Problem;
import js7.base.problem.ProblemCode;
import js7.data.event.Event;
import js7.data.event.KeyedEvent;
import js7.data.item.ItemPath;
import js7.data.item.VersionId;
import js7.data.item.VersionedEvent;
import js7.data.lock.LockPath;
import js7.data.workflow.WorkflowPath;
import js7.data_for_java.item.JUnsignedSimpleItem;
import js7.data_for_java.item.JUpdateItemOperation;
import js7.data_for_java.lock.JLock;
import js7.data_for_java.workflow.JWorkflowId;
import js7.proxy.javaapi.JControllerApi;
import js7.proxy.javaapi.JControllerProxy;
import js7.proxy.javaapi.data.controller.JEventAndControllerState;
import reactor.core.publisher.Flux;
import static java.util.Collections.singletonList;
import static java.util.concurrent.TimeUnit.SECONDS;
import static java.util.stream.Collectors.joining;
import static java.util.stream.Collectors.toList;
import static js7.data_for_java.item.JUpdateItemOperation.addOrChangeSigned;
import static js7.data_for_java.item.JUpdateItemOperation.addVersion;
import static js7.data_for_java.item.JUpdateItemOperation.deleteVersioned;
import static js7.data_for_java.vavr.VavrUtils.await;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.greaterThan;

/**
 * @author Joacim Zschimmer
 */
final class JControllerProxyRepoTester
{
    private static final WorkflowPath bWorkflowPath = WorkflowPath.of("B-WORKFLOW");  // As defined by ControllerProxyTest
    private final JControllerProxy proxy;
    private final JControllerApi api;
    private static final VersionId versionId = VersionId.of("MY-VERSION");  // Must match the versionId in added or replaced objects

    JControllerProxyRepoTester(JControllerProxy proxy) {
        this.proxy = proxy;
        this.api = proxy.api();
    }

    void addTamperedItems(List<String> manyItemJsons)
        throws InterruptedException, ExecutionException, TimeoutException
    {
        // Try to add many items with invalid signature
        String bigSpace = Stream.generate(() -> "          ").limit(10_000).collect(joining());
        assertThat(bigSpace.length(), equalTo(100_000));
        assertThat(manyItemJsons.stream().mapToInt(o -> o.length() + bigSpace.length()).sum(),
            greaterThan(100_000_000/*bytes*/));
        assertThat(
            api.updateItems(Flux.concat(
                    Flux.just(addVersion(versionId)),
                    Flux.fromIterable(manyItemJsons)
                        .map(json -> addOrChangeSigned(SignedString.of(json + bigSpace, "Silly", "MY-SILLY-FAKE")))))
                .get(99, SECONDS)
                .mapLeft(problem -> new Tuple2<>(
                    Optional.ofNullable(problem.codeOrNull()).map(ProblemCode::string),
                    problem.toString())),
            equalTo(
                Either.left(new Tuple2<>(
                    Optional.of("TamperedWithSignedMessage"),
                    "TamperedWithSignedMessage: The message does not match its signature"))));
    }

    void addItems(List<String> itemJsons)
        throws InterruptedException, ExecutionException, TimeoutException
    {
        // The specific workflow version should be unknown
        JWorkflowId workflowId = JWorkflowId.of(bWorkflowPath, versionId);
        assertThat(proxy.currentState().repo().idToWorkflow(workflowId).mapLeft(Problem::codeOrNull),
            equalTo(Either.left(ProblemCode.of("UnknownKey"/*may change*/))));

        CompletableFuture<JEventAndControllerState<Event>> whenWorkflowAdded =
            awaitEvent(keyedEvent -> isItemAdded(keyedEvent, bWorkflowPath));

        JLock lock = JLock.of(LockPath.of("MY-LOCK"), 1);
        List<JUnsignedSimpleItem> simpleItems = singletonList(lock);
        List<SignedString> signedItemJsons = itemJsons.stream().map(o -> sign(o)).collect(toList());
        // Add items
        addItemsOnly(simpleItems, signedItemJsons);

        // The repeated operation does nothing. This allows a client restart with unknown transaction state.
        addItemsOnly(simpleItems, signedItemJsons);

        whenWorkflowAdded.get(99, SECONDS);
        assertThat(proxy.currentState().repo().idToWorkflow(workflowId).map(o -> o.id().path()),
            equalTo(Either.right(bWorkflowPath)));
    }

    private void addItemsOnly(List<JUnsignedSimpleItem> simpleItems, List<SignedString> signedItemJsons) {
        await(api.updateItems(Flux.concat(
            Flux.fromIterable(simpleItems)
                .map(JUpdateItemOperation::addOrChangeSimple),
            Flux.just(addVersion(versionId)),
            Flux.fromIterable(signedItemJsons)
                .map(JUpdateItemOperation::addOrChangeSigned))));
    }

    void deleteWorkflow() throws InterruptedException, ExecutionException, TimeoutException {
        VersionId versionId = VersionId.of("MY-VERSION-2");  // Must match the versionId in added or replaced objects

        // The workflow shoud be known (latest version)
        assertThat(proxy.currentState().repo().pathToWorkflow(bWorkflowPath).isRight(), equalTo(true));

        CompletableFuture<JEventAndControllerState<Event>> whenWorkflowDeleted =
            awaitEvent(keyedEvent -> isItemDeleted(keyedEvent, bWorkflowPath));

        await(api.updateItems(Flux.just(
            addVersion(versionId),
            deleteVersioned(bWorkflowPath))));

        whenWorkflowDeleted.get(99, SECONDS);

        // The workflow should be deleted (latest version)
        assertThat(proxy.currentState().repo().pathToWorkflow(bWorkflowPath).mapLeft(Problem::codeOrNull),
            equalTo(Either.left(ProblemCode.of("VersionedItemDeleted"))));
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

    private static boolean isItemAdded(KeyedEvent<Event> keyedEvent, ItemPath path) {
        Event event = keyedEvent.event();
        return event instanceof VersionedEvent.VersionedItemAdded && ((VersionedEvent.VersionedItemAdded)event).path().equals(path);
    }

    private static boolean isItemDeleted(KeyedEvent<Event> keyedEvent, ItemPath path) {
        Event event = keyedEvent.event();
        return event instanceof VersionedEvent.VersionedItemDeleted && ((VersionedEvent.VersionedItemDeleted)event).path().equals(path);
    }
}
