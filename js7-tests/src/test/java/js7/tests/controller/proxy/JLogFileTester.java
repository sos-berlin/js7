package js7.tests.controller.proxy;

import java.time.Instant;
import java.util.List;
import java.util.concurrent.CompletionStage;
import js7.base.log.LogLevel;
import js7.proxy.javaapi.JControllerProxy;
import org.slf4j.Logger;
import static java.lang.Thread.currentThread;
import static org.slf4j.LoggerFactory.getLogger;

public final class JLogFileTester {

    private final static Logger logger = getLogger(JLogFileTester.class);

    static CompletionStage<List<String>> test(JControllerProxy proxy, String expectedLogText) {
        return proxy.engineLog().use(engineLog -> {
            logger.info(currentThread().getName());
            assert currentThread().getName().startsWith("ForkJoinPool.commonPool-");
            return engineLog
                .logSection(LogLevel.debug(), Instant.now().minusSeconds(3), Integer.MAX_VALUE)
                .doOnNext(line -> {
                    logger.info("➤ " + line.stripTrailing());
                    assert currentThread().getName().startsWith("ForkJoinPool.commonPool-");
                })
                .takeUntil(line -> line.contains(expectedLogText))
                .collectList() // 💥 May blow up the heap
                .toFuture();
        });
    }

    // Same as test above, but without assertions or logging
    static CompletionStage<List<String>> prettyTest(JControllerProxy proxy, String expectedLogText) {
        return proxy.engineLog().use(engineLog -> engineLog
            .logSection(LogLevel.debug(), Instant.now().minusSeconds(3), Integer.MAX_VALUE)
            .takeUntil(line -> line.contains(expectedLogText))
            .collectList() // 💥 May blow up the heap
            .toFuture());
    }
}
