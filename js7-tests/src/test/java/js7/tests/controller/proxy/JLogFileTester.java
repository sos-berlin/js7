package js7.tests.controller.proxy;

import com.typesafe.config.ConfigFactory;
import java.time.Instant;
import java.util.List;
import java.util.OptionalLong;
import java.util.concurrent.CompletableFuture;
import java.util.function.Function;
import js7.base.log.LogLevel;
import js7.base.log.reader.KeyedLogLine;
import js7.data.node.EngineServerId;
import js7.data_for_java.auth.JAdmission;
import js7.data_for_java.auth.JHttpsConfig;
import js7.proxy.javaapi.JControllerProxy;
import js7.proxy.javaapi.JProxyContext;
import org.slf4j.Logger;
import reactor.core.scheduler.Schedulers;
import static java.lang.Thread.currentThread;
import static java.util.function.Function.identity;
import static js7.proxy.javaapi.JProxyContext.assertIsProxyThread;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.startsWith;
import static org.slf4j.LoggerFactory.getLogger;

final class JLogFileTester {

    private final static Logger logger = getLogger(JLogFileTester.class);

    static CompletableFuture<Result> test(JControllerProxy proxy, String expectedLogText) {
        assertIsProxyThread(); // Due to JLogFileTest
        return proxy
            .keyedLogLineFlux(
                EngineServerId.primaryController,
                LogLevel.info(),
                Instant.now().minusSeconds(3),
                OptionalLong.empty()/*special case for test*/)
            .flatMapIterable(identity())
            .doOnNext(keyedLogLine ->
                assertIsProxyThread())
            .takeUntil(keyedLogLine -> keyedLogLine.line().contains(expectedLogText))
            .collectList() // 💥 May blow up the heap
            .toFuture()
            .thenCompose(keyedLogLines -> {
                //Java 21: var lastKey = keyedLogLines.getLast().key();
                var lastKey = keyedLogLines.get(keyedLogLines.size() - 1).key();
                return proxy.keyedLogLineFlux(
                        EngineServerId.primaryController,
                        LogLevel.info(),
                        lastKey,
                        /*lines=*/OptionalLong.of(2))
                    .flatMapIterable(identity())
                    .map(KeyedLogLine::removeHighlights) // Slow
                    .collectList()
                    .toFuture()
                    .thenApply(moreLines ->
                        new Result(
                            keyedLogLines.stream().map(KeyedLogLine::line).toList(),
                            moreLines.stream().map(KeyedLogLine::line).toList()));
            });
    }

    // Same as test as above, but without assertions or logging
    static CompletableFuture<Long> prettyTestNonBlocking(List<JAdmission> admissions) {
        assertThat(currentThread().getName(), startsWith("ForkJoinPool.commonPool-worker-"));
        return runWithProxy(admissions, controllerProxy -> {
            assertIsProxyThread();
            return prettyTestNonBlocking(controllerProxy);
        });
    }

    // Same as test as above, but without assertions or logging
    static CompletableFuture<Long> prettyTestNonBlocking(JControllerProxy controllerProxy) {
        return controllerProxy
            .keyedLogLineFlux(
                EngineServerId.primaryController,
                LogLevel.info(),
                Instant.now().minusSeconds(3600),
                /*lines=*/OptionalLong.empty()/*special case for test*/)
            .doOnNext(ignore ->
                assertIsProxyThread()) // Do not block here!
            // 8% slower: .flatMapIterable(identity())
            .doOnNext(keyedLogLines -> {
                for (var keyedLogLine: keyedLogLines)
                    doSomethingNonBlocking(keyedLogLine.line());
            })
            .reduce(0L, (a, lines) -> a + lines.size()) // Count lines
            .toFuture();
    }

    static void doSomethingNonBlocking(String line) {
    }

    static CompletableFuture<Long> prettyTestBlocking(List<JAdmission> admissions) {
        assertThat(currentThread().getName(), startsWith("ForkJoinPool.commonPool-worker-"));
        return runWithProxy(admissions, controllerProxy -> {
            assertIsProxyThread();
            return prettyTestBlocking(controllerProxy);
        });
    }

    static CompletableFuture<Long> prettyTestBlocking(JControllerProxy controllerProxy) {
        return controllerProxy
            .keyedLogLineFlux(
                EngineServerId.primaryController,
                LogLevel.info(),
                Instant.now().minusSeconds(3600),
                /*lines=*/OptionalLong.empty()/*special case for test*/)
            // Switch to a blocking thread pool. Apparently not slower.
            .publishOn(Schedulers.boundedElastic()/*<--READ THE DOC !!!*/)
            .doOnNext(ignore ->
                assertThat(currentThread().getName(), startsWith("boundedElastic-")))
            .flatMapIterable(identity())
            .doOnNext(keyedLogLine ->
                doSomethingBlocking(keyedLogLine.line()))
            .count().toFuture();
    }

    private static void doSomethingBlocking(String line) {
    }

    static CompletableFuture<Long> testRawNonBlocking(JControllerProxy controllerProxy) {
        return controllerProxy
            .rawLogLineFlux(
                EngineServerId.primaryController,
                LogLevel.info(),
                Instant.now().minusSeconds(3600),
                /*lines=*/OptionalLong.empty()/*special case for test*/)
            .doOnNext(ignore ->
                assertIsProxyThread()) // Do not block here!
            .doOnNext(lines -> {
                for (var line: lines) {
                    doSomethingWithRawLineNonBlocking(line);
                }
            })
            .reduce(0L, (a, lines) -> a + lines.size()) // Count lines
            .toFuture();
    }

    private static void doSomethingWithRawLineNonBlocking(byte[] line) {
    }

    /**
     * Start and stop JProxyContext, JControllerApi and JControllerProxy.
     */
    private static <A> CompletableFuture<A> runWithProxy(
        List<JAdmission> admissions,
        Function<JControllerProxy, CompletableFuture<A>> body) {
        return
            JProxyContext.run(ConfigFactory.empty(), jProxyContext ->
                jProxyContext.runControllerApi(admissions, JHttpsConfig.empty(), jControllerApi ->
                    jControllerApi.runControllerProxy(body)));
    }


    record Result(List<String> firstList, List<String> secondList) {
    }
}
