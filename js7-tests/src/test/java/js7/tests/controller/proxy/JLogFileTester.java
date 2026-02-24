package js7.tests.controller.proxy;

import java.time.Instant;
import java.util.LinkedList;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionStage;
import js7.base.log.LogLevel;
import js7.proxy.javaapi.JControllerProxy;
import js7.proxy.javaapi.JEngineLog;

public final class JLogFileTester  {

    static CompletionStage<List<String>> test(JControllerProxy proxy, String expectedLogText)
        throws Exception
    {
        var foundPromise = new CompletableFuture<Void>();
        var lines = new LinkedList<String>();
        return
            proxy.engineLog().thenComposeAsync(
                allocated -> {
                    JEngineLog engineLog = allocated.allocatedThing();
                    return engineLog
                        .logSection(LogLevel.debug(), Instant.now().minusSeconds(3), /*lines=*/Integer.MAX_VALUE)
                        .thenApply(flux -> {
                            flux
                            .takeUntil(line -> {
                                var found = line.contains(expectedLogText);
                                if (found) foundPromise.complete(null);
                                return found;
                            })
                            .subscribe(
                                line -> lines.add(line),
                                foundPromise::completeExceptionally);
                            return allocated;
                        });
                })
                .whenComplete((allocated, throwable) ->
                    // FIXME release() ist ein CompletableState. Wie binden wir den ein?
                    allocated.release()) // Runs asynchronously, for now
                .thenCompose(ignore -> foundPromise)
                .thenApply(x -> lines);
    }
}
