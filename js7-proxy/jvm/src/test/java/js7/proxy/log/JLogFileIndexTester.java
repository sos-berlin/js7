package js7.proxy.log;

import com.typesafe.config.ConfigFactory;
import java.nio.file.Path;
import java.time.Instant;
import java.time.ZoneId;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;
import js7.proxy.javaapi.JProxyContext;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.startsWith;

public final class JLogFileIndexTester {

    static CompletableFuture<Void> test(Path file, ZoneId zoneId, List<String> lines) {
        return
            JProxyContext.run(
                ConfigFactory.empty(),
                proxyContext ->
                    test1(proxyContext, file, zoneId, lines).thenCompose(ignore ->
                        test2(proxyContext, file, zoneId)));
    }

    static CompletableFuture<Void> test1(JProxyContext proxyContext, Path file, ZoneId zoneId, List<String> lines) {
        return
            JLogFileIndex.build(proxyContext, file, zoneId).thenCompose(logFileIndex ->
                logFileIndex.instantToFilePosition(Instant.parse("2026-02-12T14:00:01+02:00"))
                    .thenAccept(position ->
                        assertThat(position.getAsLong(), equalTo((long)lines.get(0).length()/*pure ASCII*/))));
    }

    static CompletableFuture<Void> test2(JProxyContext proxyContext, Path file, ZoneId zoneId) {
        return
            JLogFileIndex.build(proxyContext, file, zoneId).thenCompose(logFileIndex ->
                logFileIndex
                    .lineFlux(
                        Instant.parse("2026-02-12T14:00:01+02:00"),
                        Optional.of(Instant.parse("2026-02-12T14:00:04+02:00"))
                    ).take(100) // Guard against too many lines
                    .collectList()
                    .toFuture()
            ).thenAccept(lines -> {
                assertThat(lines.get(0), startsWith("2026-02-12 14:00:01.000 "));
                assertThat(lines.get(1), startsWith("2026-02-12 14:00:02.000 "));
                assertThat(lines.get(2), startsWith("2026-02-12 14:00:03.000 "));
                assertThat(lines.size(), equalTo(3));
            });
    }
}
