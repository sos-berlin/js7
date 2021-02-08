package js7.executor.forjava.internal.tests;

import java.util.concurrent.CompletableFuture;
import js7.executor.forjava.internal.JInternalJob;
import js7.executor.forjava.internal.JOrderContext;
import js7.executor.forjava.internal.JOrderProcess;
import js7.executor.forjava.internal.JOrderResult;
import static java.util.Collections.emptyMap;

/** Skeleton for a JInternalJob implementation. */
public final class EmptyJInternalJob implements JInternalJob
{
    public JOrderProcess processOrder(JOrderContext context) {
        return JOrderProcess.of(
            CompletableFuture.supplyAsync(
                () -> JOrderResult.of(emptyMap())));
    }
}
