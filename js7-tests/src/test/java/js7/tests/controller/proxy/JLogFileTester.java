package js7.tests.controller.proxy;

import java.time.Instant;
import java.util.List;
import java.util.concurrent.CompletableFuture;
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

    static CompletableFuture<List<String>> test(JControllerProxy proxy, String expectedLogText) {
        return proxy
            .logSection(LogLevel.debug(), Instant.now().minusSeconds(3), Integer.MAX_VALUE)
            .doOnNext(line -> {
                logger.info("➤ " + line.stripTrailing());
                assertThat(currentThread().getName(), not(startsWith(JProxyContext.ThreadPrefix)));
            })
            .takeUntil(line -> line.contains(expectedLogText))
            .collectList() // 💥 May blow up the heap
            .toFuture();
    }

    // Same as test as above, but without assertions or logging
    static CompletableFuture<Void> prettyTest(JControllerProxy proxy) {
        return proxy.logSection(LogLevel.debug(), Instant.now().minusSeconds(3), /*lines=*/3)
            .doOnNext(line ->
                doSomethingNonBlocking(line)
                //
                // Bei blockierendem Code (I/O, sleep, lock) sollte ein Executor mit einem
                // Threadpool für blockierende Threads gewählt werden.
                // Sonst kann sich der commonPool verklemmen.
            )
            .then().toFuture();
    }

    static void doSomethingNonBlocking(String line) {}
}
