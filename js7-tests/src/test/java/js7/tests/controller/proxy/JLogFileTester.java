package js7.tests.controller.proxy;

import java.time.Instant;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import js7.base.log.KeyedLogLine;
import js7.base.log.LogLevel;
import js7.proxy.javaapi.JControllerProxy;
import js7.proxy.javaapi.JProxyContext;
import org.slf4j.Logger;
import static java.lang.Thread.currentThread;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.not;
import static org.hamcrest.Matchers.startsWith;
import static org.slf4j.LoggerFactory.getLogger;

public final class JLogFileTester {

    private final static Logger logger = getLogger(JLogFileTester.class);

    static CompletableFuture<Result> test(JControllerProxy proxy, String expectedLogText) {
        String myThreadPrefix = currentThread().getName().replaceFirst("(?-)[^-]+$", "");
        //FIXME assertThat(myThreadPrefix, not(startsWith(JProxyContext.ThreadPrefix)));
        return proxy
            .logSection(LogLevel.debug(), Instant.now().minusSeconds(3), Integer.MAX_VALUE)
            .doOnNext(keyedLogLine -> {
                logger.info("➤ " + keyedLogLine.line().stripTrailing());
                //FIXME assertThat(currentThread().getName(), not(startsWith(JProxyContext.ThreadPrefix)));
                //FIXME assertThat(currentThread().getName(), startsWith(myThreadPrefix));
            })
            .takeUntil(keyedLogLine -> keyedLogLine.line().contains(expectedLogText))
            .collectList() // 💥 May blow up the heap
            .toFuture()
            .thenCompose(keyedLogLines -> {
                //Java 21: var lastKey = keyedLogLines.getLast().key();
                var lastKey = keyedLogLines.get(keyedLogLines.size() - 1).key();
                return proxy.logSection(LogLevel.debug(), lastKey, /*lines=*/2)
                    .collectList()
                    .toFuture()
                    .thenApply(moreLines ->
                        new Result(
                            keyedLogLines.stream().map(KeyedLogLine::line).toList(),
                            moreLines.stream().map(KeyedLogLine::line).toList()));
            });
    }

    // Same as test as above, but without assertions or logging
    static CompletableFuture<Void> prettyTest(JControllerProxy proxy) {
        return proxy.logSection(LogLevel.debug(), Instant.now().minusSeconds(3), /*lines=*/3)
            .doOnNext(keyedLogLine ->
                doSomethingNonBlocking(keyedLogLine.line())
                //
                // Bei blockierendem Code (I/O, sleep, lock) sollte ein Executor mit einem
                // Threadpool für blockierende Threads gewählt werden.
                // Sonst kann sich der commonPool verklemmen.
            )
            .then().toFuture();
    }

    static void doSomethingNonBlocking(String line) {}

    record Result(List<String> firstList, List<String> secondList) {}
}
