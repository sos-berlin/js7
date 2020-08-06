package js7.tests.controller.proxy;

import io.vavr.control.Either;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;
import java.util.function.Predicate;
import js7.base.crypt.SignedString;
import js7.base.problem.Problem;
import js7.base.problem.ProblemCode;
import js7.data.event.Event;
import js7.data.event.KeyedEvent;
import js7.data.item.RepoEvent;
import js7.data.item.TypedPath;
import js7.data.item.VersionId;
import js7.data.workflow.WorkflowPath;
import js7.proxy.javaapi.JControllerProxy;
import js7.proxy.javaapi.JEventAndControllerState;
import js7.proxy.javaapi.data.JUpdateRepoOperation;
import js7.proxy.javaapi.data.JWorkflowId;
import reactor.core.publisher.Flux;
import static java.util.Arrays.asList;
import static java.util.concurrent.TimeUnit.SECONDS;
import static js7.proxy.javaapi.utils.VavrUtils.getOrThrow;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.MatcherAssert.assertThat;

/**
 * @author Joacim Zschimmer
 */
final class JControllerProxyRepoTester
{
    private static final WorkflowPath bWorkflowPath = WorkflowPath.of("/B-WORKFLOW");  // As defined by ControllerProxyTest
    private final JControllerProxy proxy;

    JControllerProxyRepoTester(JControllerProxy proxy) {
        this.proxy = proxy;
    }

    void addItems(List<String> itemJsons) throws InterruptedException, ExecutionException, TimeoutException {
        VersionId versionId = VersionId.of("MY-VERSION");  // Must match the versionId in added or replaced objects

        // The specific workflow version should be unknown
        JWorkflowId workflowId = JWorkflowId.of(bWorkflowPath, versionId);
        assertThat(proxy.currentState().idToWorkflow(workflowId).mapLeft(Problem::codeOrNull),
            equalTo(Either.left(ProblemCode.of("UnknownKey"/*may change*/))));

        CompletableFuture<JEventAndControllerState<Event>> whenWorkflowAdded =
            awaitEvent(keyedEvent -> isItemAdded(keyedEvent, bWorkflowPath));

        // Add items
        getOrThrow(proxy.api()
            .updateRepo(
                versionId,
                Flux.fromStream(
                    itemJsons.stream()
                        .map(o -> JUpdateRepoOperation.addOrReplace(sign(o)))))
            .get(99, SECONDS));

        whenWorkflowAdded.get(99, SECONDS);
        assertThat(proxy.currentState().idToWorkflow(workflowId).map(o -> o.id().path()),
            equalTo(Either.right(bWorkflowPath)));
    }

    void deleteItem() throws InterruptedException, ExecutionException, TimeoutException {
        VersionId versionId = VersionId.of("MY-VERSION-2");  // Must match the versionId in added or replaced objects

        // The workflow shoud be known (latest version)
        assertThat(proxy.currentState().pathToWorkflow(bWorkflowPath).isRight(), equalTo(true));

        CompletableFuture<JEventAndControllerState<Event>> whenWorkflowDeleted =
            awaitEvent(keyedEvent -> isItemDeleted(keyedEvent, bWorkflowPath));

        getOrThrow(proxy.api()
            .updateRepo(
                versionId,
                Flux.fromIterable(asList(
                    JUpdateRepoOperation.delete(bWorkflowPath))))
            .get(99, SECONDS));

        whenWorkflowDeleted.get(99, SECONDS);

        // The workflow should be deleted (latest version)
        assertThat(proxy.currentState().pathToWorkflow(bWorkflowPath).mapLeft(Problem::codeOrNull),
            equalTo(Either.left(ProblemCode.of("ItemDeleted"))));
    }

    private static SignedString sign(String json) {
        return SignedString.of(
            json,                       // The string to be be signed
            "Silly",                    // Thy signature type, "PGP" (or "Silly" for silly testing)
            "MY-SILLY-SIGNATURE");      // The signature of string
    }

    private CompletableFuture<JEventAndControllerState<Event>> awaitEvent(Predicate<KeyedEvent<Event>> predicate) {
        return proxy
            .flux()
            .skipWhile(o -> !predicate.test(o.stampedEvent().value()))
            .elementAt(0)
            .toFuture();
    }

    private static boolean isItemAdded(KeyedEvent<Event> keyedEvent, TypedPath path) {
        Event event = keyedEvent.event();
        return event instanceof RepoEvent.ItemAdded && ((RepoEvent.ItemAdded)event).path().equals(path);
    }

    private static boolean isItemDeleted(KeyedEvent<Event> keyedEvent, TypedPath path) {
        Event event = keyedEvent.event();
        return event instanceof RepoEvent.ItemDeleted && ((RepoEvent.ItemDeleted)event).path().equals(path);
    }
}
