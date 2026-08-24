package js7.tests.controller.proxy.log;

import java.nio.file.Path;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.util.List;
import java.util.Set;
import java.util.concurrent.CompletableFuture;
import js7.base.log.LogLevel;
import js7.base.log.reader.LogLineKey;
import js7.proxy.javaapi.JProxyContext;
import js7.proxy.javaapi.log.JLogDirectoryIndex;
import js7.proxy.javaapi.log.JLogSelection;
import reactor.core.publisher.Flux;
import static java.util.Arrays.asList;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;

final class JLogDirectoryIndexTester {
    private JLogDirectoryIndexTester() {}

    private static final List<String> expectedLines = asList(
        "2026-03-01 00:00:00.000+02 Begin ...\n",
        "2026-03-01 00:00:01.000+02 info JLogDirectoryIndexTest - MESSAGE 1\n",
        "2026-03-01 00:00:02.000+02 info JLogDirectoryIndexTest - MESSAGE 2\n",
        "2026-03-01 00:00:03.000+02 info JLogDirectoryIndexTest - MESSAGE 3\n",

        "2026-03-01 01:00:00.000+02 Begin ...\n",
        "2026-03-01 01:00:01.000+02 info JLogDirectoryIndexTest - MESSAGE 4\n",
        "2026-03-01 01:00:02.000+02 info JLogDirectoryIndexTest - MESSAGE 5\n",
        "2026-03-01 01:00:03.000+02 info JLogDirectoryIndexTest - MESSAGE 6\n",

        "2026-03-01 02:00:00.000+02 Begin ...\n",
        "2026-03-01 02:00:01.000+02 info JLogDirectoryIndexTest - MESSAGE 7\n",
        "2026-03-01 02:00:02.000+02 info JLogDirectoryIndexTest - MESSAGE 8\n",
        "2026-03-01 02:00:03.000+02 info JLogDirectoryIndexTest - MESSAGE 9\n",

        "2026-03-02 00:00:00.000+02 Begin ...\n",
        "2026-03-02 00:00:01.000+02 info JLogDirectoryIndexTest - MESSAGE 10\n",
        "2026-03-02 00:00:02.000+02 info JLogDirectoryIndexTest - MESSAGE 11\n",
        "2026-03-02 00:00:03.000+02 info JLogDirectoryIndexTest - MESSAGE 12\n",

        "2026-03-02 01:00:00.000+02 Begin ...\n",
        "2026-03-02 01:00:01.000+02 info JLogDirectoryIndexTest - MESSAGE 13\n",
        "2026-03-02 01:00:02.000+02 info JLogDirectoryIndexTest - MESSAGE 14\n",
        "2026-03-02 01:00:03.000+02 info JLogDirectoryIndexTest - MESSAGE 15\n",

        "2026-03-02 02:00:00.000+02 Begin ...\n",
        "2026-03-02 02:00:01.000+02 info JLogDirectoryIndexTest - MESSAGE 16\n",
        "2026-03-02 02:00:02.000+02 info JLogDirectoryIndexTest - MESSAGE 17\n",
        "2026-03-02 02:00:03.000+02 info JLogDirectoryIndexTest - MESSAGE 18\n",

        "2026-03-03 00:00:00.000+02 Begin ...\n",
        "2026-03-03 00:00:01.000+02 info JLogDirectoryIndexTest - MESSAGE 19\n",
        "2026-03-03 00:00:02.000+02 info JLogDirectoryIndexTest - MESSAGE 20\n",
        "2026-03-03 00:00:03.000+02 info JLogDirectoryIndexTest - MESSAGE 21\n",

        "2026-03-03 01:00:00.000+02 Begin ...\n",
        "2026-03-03 01:00:01.000+02 info JLogDirectoryIndexTest - MESSAGE 22\n",
        "2026-03-03 01:00:02.000+02 info JLogDirectoryIndexTest - MESSAGE 23\n",
        "2026-03-03 01:00:03.000+02 info JLogDirectoryIndexTest - MESSAGE 24\n",

        "2026-03-03 02:00:00.000+02 Begin ...\n",
        "2026-03-03 02:00:01.000+02 info JLogDirectoryIndexTest - MESSAGE 25\n",
        "2026-03-03 02:00:02.000+02 info JLogDirectoryIndexTest - MESSAGE 26\n",
        "2026-03-03 02:00:03.000+02 info JLogDirectoryIndexTest - MESSAGE 27\n");

    static CompletableFuture<Void> test(JProxyContext jProxyContext, ZoneId zoneId, Path directory,
                                        LogLevel logLevel) {
        var logFilePrefix = "PREFIX";
        return
            // Allocate one a JLogDirectoryIndex for the log directory
            // The JLogDirectoryIndex provides a LogStreamIndex for each log file prefix and LogLevel.
            // LogStreamIndex cannot be closed, because it's managed by JLogDirectoryIndex
            // Instead of the .use method, one can use the .allocated method and then release manually
            JLogDirectoryIndex.directory(
                directory, Set.of(logFilePrefix), zoneId, jProxyContext
            ).use(logDirectoryIndex -> // Keep logDirectoryIndex and use it for all log file accesses
                logDirectoryIndex.logStreamIndex(logFilePrefix, logLevel).thenCompose(logStreamIndex ->
                    logStreamIndex
                        // 🟢 instantToLogLineKey
                        .instantToLogLineKey(
                            ZonedDateTime.parse("2026-03-01T02:00:02+02").toInstant(),
                            JLogSelection.all())
                        .thenAccept(logLineKey ->
                            assertThat(logLineKey.get(), equalTo(LogLineKey.apply(
                                ZonedDateTime.parse("2026-03-01T02:00:01+02").toInstant(),
                                expectedLines.get(0).length() + expectedLines.get(1).length()))))
                        .thenCompose(unused ->
                            // 🟢 keyedLogLineFlux
                            logStreamIndex.keyedLogLineFlux(
                                    ZonedDateTime.parse("2026-01-01T00:00:00+02").toInstant(),
                                    JLogSelection.all())
                                .flatMap(Flux::fromIterable)
                                .map(keyedLogLine -> keyedLogLine.line())
                                .collectList().toFuture())
                        .thenAccept(lines ->
                            assertThat(lines, equalTo(expectedLines)))));
    }
}
