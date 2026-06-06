package js7.tests.controller.proxy.log;

import java.nio.file.Path;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.util.concurrent.CompletableFuture;
import js7.base.log.LogLevel;
import js7.base.log.reader.LogLineKey;
import js7.proxy.javaapi.JProxyContext;
import js7.proxy.javaapi.log.JLogDirectoryIndex;
import js7.proxy.javaapi.log.JLogSelection;
import static java.util.Arrays.asList;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;

final class JLogDirectoryIndexTester {
    private JLogDirectoryIndexTester() {}

    static CompletableFuture<Void> test(JProxyContext jProxyContext, ZoneId zoneId, Path directory) {
        LogLevel logLevel = LogLevel.info(); // Some LogLevel we think is appropriate
        return
            JLogDirectoryIndex.directory(
                directory, path -> true, LogLevel.info(), zoneId, jProxyContext
            ).use(logDirectoryIndex ->
                logDirectoryIndex
                    // 🟢 instantToLogLineKey
                    .instantToLogLineKey(
                        ZonedDateTime.parse("2026-03-01T02:00:02+02").toInstant(),
                        JLogSelection.empty())
                    .thenAccept(logLineKey ->
                        assertThat(logLineKey.get(), equalTo(LogLineKey.apply(
                            logLevel,
                            ZonedDateTime.parse("2026-03-01T02:00:00+02").toInstant(),
                            110))))

                    // 🟢 keyedLogLineFlux
                    .thenCompose(unused ->
                        logDirectoryIndex.keyedLogLineFlux(
                                ZonedDateTime.parse("2026-01-01T00:00:00+02").toInstant(),
                                JLogSelection.empty())
                            .map(keyedLogLine -> keyedLogLine.line())
                            .collectList().toFuture())
                    .thenAccept(lines ->
                        assertThat(lines, equalTo(asList(
                            //"2026-03-01 00:00:00.000+0200 HEADER\n",
                            "2026-03-01 00:00:01.000+02 info LogDirectoryIndexTest - MESSAGE 1\n",
                            "2026-03-01 00:00:02.000+02 info LogDirectoryIndexTest - MESSAGE 2\n",
                            "2026-03-01 00:00:03.000+02 info LogDirectoryIndexTest - MESSAGE 3\n",

                            //"2026-03-01 01:00:00.000+0200 HEADER\n",
                            "2026-03-01 01:00:01.000+02 info LogDirectoryIndexTest - MESSAGE 4\n",
                            "2026-03-01 01:00:02.000+02 info LogDirectoryIndexTest - MESSAGE 5\n",
                            "2026-03-01 01:00:03.000+02 info LogDirectoryIndexTest - MESSAGE 6\n",

                            //"2026-03-01 02:00:00.000+0200 HEADER\n",
                            "2026-03-01 02:00:01.000+02 info LogDirectoryIndexTest - MESSAGE 7\n",
                            "2026-03-01 02:00:02.000+02 info LogDirectoryIndexTest - MESSAGE 8\n",
                            "2026-03-01 02:00:03.000+02 info LogDirectoryIndexTest - MESSAGE 9\n",

                            //"2026-03-02 00:00:00.000+0200 HEADER\n",
                            "2026-03-02 00:00:01.000+02 info LogDirectoryIndexTest - MESSAGE 10\n",
                            "2026-03-02 00:00:02.000+02 info LogDirectoryIndexTest - MESSAGE 11\n",
                            "2026-03-02 00:00:03.000+02 info LogDirectoryIndexTest - MESSAGE 12\n",

                            //"2026-03-02 01:00:00.000+0200 HEADER\n",
                            "2026-03-02 01:00:01.000+02 info LogDirectoryIndexTest - MESSAGE 13\n",
                            "2026-03-02 01:00:02.000+02 info LogDirectoryIndexTest - MESSAGE 14\n",
                            "2026-03-02 01:00:03.000+02 info LogDirectoryIndexTest - MESSAGE 15\n",

                            //"2026-03-02 02:00:00.000+0200 HEADER\n",
                            "2026-03-02 02:00:01.000+02 info LogDirectoryIndexTest - MESSAGE 16\n",
                            "2026-03-02 02:00:02.000+02 info LogDirectoryIndexTest - MESSAGE 17\n",
                            "2026-03-02 02:00:03.000+02 info LogDirectoryIndexTest - MESSAGE 18\n",

                            //"2026-03-03 00:00:00.000+0200 HEADER\n",
                            "2026-03-03 00:00:01.000+02 info LogDirectoryIndexTest - MESSAGE 19\n",
                            "2026-03-03 00:00:02.000+02 info LogDirectoryIndexTest - MESSAGE 20\n",
                            "2026-03-03 00:00:03.000+02 info LogDirectoryIndexTest - MESSAGE 21\n",

                            //"2026-03-03 01:00:00.000+0200 HEADER\n",
                            "2026-03-03 01:00:01.000+02 info LogDirectoryIndexTest - MESSAGE 22\n",
                            "2026-03-03 01:00:02.000+02 info LogDirectoryIndexTest - MESSAGE 23\n",
                            "2026-03-03 01:00:03.000+02 info LogDirectoryIndexTest - MESSAGE 24\n",

                            //"2026-03-03 02:00:00.000+0200 HEADER\n",
                            "2026-03-03 02:00:01.000+02 info LogDirectoryIndexTest - MESSAGE 25\n",
                            "2026-03-03 02:00:02.000+02 info LogDirectoryIndexTest - MESSAGE 26\n",
                            "2026-03-03 02:00:03.000+02 info LogDirectoryIndexTest - MESSAGE 27\n")))));
    }
}
