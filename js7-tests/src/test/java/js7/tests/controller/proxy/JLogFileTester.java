package js7.tests.controller.proxy;

import com.typesafe.config.ConfigFactory;
import java.time.Instant;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;
import java.util.function.Function;
import js7.base.log.LogLevel;
import js7.base.log.reader.KeyedLogLine;
import js7.data.node.Js7ServerId;
import js7.data_for_java.auth.JAdmission;
import js7.data_for_java.auth.JHttpsConfig;
import js7.proxy.javaapi.JControllerProxy;
import js7.proxy.javaapi.JProxyContext;
import js7.proxy.javaapi.log.JLogSelection;
import org.slf4j.Logger;
import reactor.core.scheduler.Schedulers;
import static java.lang.Thread.currentThread;
import static java.util.function.Function.identity;
import static js7.proxy.javaapi.JProxyContext.assertIsNotProxyThread;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.startsWith;
import static org.slf4j.LoggerFactory.getLogger;

final class JLogFileTester {

    private final static Logger logger = getLogger(JLogFileTester.class);

    static CompletableFuture<Result> test(JControllerProxy proxy, String expectedLogText) {
        //assertIsProxyThread(); // Due to JLogFileTest
        return proxy
            .keyedLogLineFlux(
                Js7ServerId.primaryController,
                LogLevel.info(),
                Instant.now().minusSeconds(3),
                JLogSelection.empty()/*special case for test*/)
            //.doOnNext(chunk ->
            //    assertIsProxyThread())                                                                                                                                               0
            .flatMapIterable(identity())
            .takeUntil(keyedLogLine -> keyedLogLine.line().contains(expectedLogText))
            .collectList() // 💥 May blow up the heap
            .toFuture()
            .thenCompose(keyedLogLines -> {
                //Java 21: var lastKey = keyedLogLines.getLast().key();
                var lastKey = keyedLogLines.get(keyedLogLines.size() - 1).key();
                return proxy.keyedLogLineFlux(
                        Js7ServerId.primaryController,
                        LogLevel.info(),
                        lastKey,
                        JLogSelection.empty().withLineLimit(2))
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
            //assertIsProxyThread();
            return prettyTestNonBlocking(controllerProxy);
        });
    }

    // Same as test as above, but without assertions or logging
    static CompletableFuture<Long> prettyTestNonBlocking(JControllerProxy controllerProxy) {
        return controllerProxy
            .keyedLogLineFlux(
                Js7ServerId.primaryController,
                LogLevel.info(),
                Instant.now().minusSeconds(3600),
                JLogSelection.empty()/*beware: no lineLimit here*/)
            //.doOnNext(ignore ->
            //    assertIsProxyThread()) // Do not block here!
            // 8% slower: .flatMapIterable(identity())
            .doOnNext(keyedLogLines -> {
                for (var keyedLogLine: keyedLogLines)
                    doSomething(keyedLogLine.line());
            })
            .reduce(0L, (a, lines) -> a + lines.size()) // Count lines
            .toFuture();
    }

    static CompletableFuture<Long> prettyTestBlocking(List<JAdmission> admissions) {
        assertThat(currentThread().getName(), startsWith("ForkJoinPool.commonPool-worker-"));
        return runWithProxy(admissions, controllerProxy -> {
            //assertIsProxyThread();
            return prettyTestBlocking(controllerProxy);
        });
    }

    static CompletableFuture<Long> prettyTestBlocking(JControllerProxy controllerProxy) {
        return controllerProxy
            .keyedLogLineFlux(
                Js7ServerId.primaryController,
                LogLevel.info(),
                Instant.now().minusSeconds(3600),
                JLogSelection.empty()/*special case for test*/)
            // Switch to a blocking thread pool. Apparently not slower.
            .publishOn(Schedulers.boundedElastic()/*<--READ THE DOC !!!*/)
            .doOnNext(ignore ->
                assertThat(currentThread().getName(), startsWith("boundedElastic-")))
            .flatMapIterable(identity())
            .doOnNext(keyedLogLine ->
                doSomething(keyedLogLine.line()))
            .count().toFuture();
    }

    static CompletableFuture<String> testScrolling(List<JAdmission> admissions) {
        return runWithProxy(admissions, controllerProxy ->
            controllerProxy
                // Read the lines
                .keyedLogLineFlux(
                    Js7ServerId.primaryController,
                    LogLevel.info(),
                    Instant.now().minusSeconds(3600),
                    JLogSelection.empty().withLineLimit(3))
                .publishOn(Schedulers.boundedElastic())
                .flatMapIterable(identity())
                .last()
                .flatMap(last ->
                    // Continue reading with the last of the three read log lines
                    controllerProxy
                        .keyedLogLineFlux(
                            Js7ServerId.primaryController,
                            LogLevel.info(),
                            last.key(),
                            JLogSelection.empty().withLineLimit(1))
                        .flatMapIterable(identity())
                        .last()
                        .map(next -> {
                            // First line is the last of the three read log lines
                            assert next.equals(last);
                            return "FINISHED";
                        }))
                .toFuture());
    }

    static CompletableFuture<Long> testRawBlocking(JControllerProxy controllerProxy) {
        return controllerProxy
            .byteLogLineFlux(
                Js7ServerId.primaryController,
                LogLevel.info(),
                Instant.now().minusSeconds(3600),
                JLogSelection.empty()/*special case, otherwise set lineLimit!*/)
            .publishOn(Schedulers.boundedElastic()/*<--READ THE DOC !!!*/)
            .doOnNext(ignore ->
                assertIsNotProxyThread())
            .doOnNext(lines -> {
                for (var line: lines) {
                    doSomething(line);
                }
            })
            .reduce(0L, (a, lines) -> a + lines.size()) // Count lines
            .toFuture();
    }

    static CompletableFuture<Long> testRawNonBlocking(JControllerProxy controllerProxy) {
        return controllerProxy
            .byteLogLineFlux(
                Js7ServerId.primaryController,
                LogLevel.info(),
                Instant.now().minusSeconds(3600),
                JLogSelection.empty()/*special case, otherwise set lineLimit!*/)
            //.doOnNext(ignore ->
            //    assertIsProxyThread()) // Do not block here!
            .doOnNext(lines -> {
                for (var line: lines) {
                    doSomething(line);
                }
            })
            .reduce(0L, (a, lines) -> a + lines.size()) // Count lines
            .toFuture();
    }

    private static void doSomething(Object line) {
    }

    /**
     * Start and stop JProxyContext, JControllerApi and JControllerProxy.
     */
    private static <A> CompletableFuture<A> runWithProxy(
        List<JAdmission> admissions,
        Function<JControllerProxy, CompletableFuture<A>> body) {
        return
            JProxyContext.run(Optional.empty(), ConfigFactory.empty(), jProxyContext ->
                jProxyContext.runControllerApi(
                    admissions,
                    JHttpsConfig.empty(),
                    jControllerApi ->
                        jControllerApi.runControllerProxy(body)));
    }


    record Result(List<String> firstList, List<String> secondList) {
    }
}
