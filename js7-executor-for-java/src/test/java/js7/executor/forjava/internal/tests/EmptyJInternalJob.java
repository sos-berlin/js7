package js7.executor.forjava.internal.tests;

import java.util.concurrent.CompletableFuture;
import js7.data_for_java.order.JOutcome;
import js7.executor.forjava.internal.JInternalJob;
import js7.executor.forjava.internal.JOrderProcess;

/** Skeleton for a JInternalJob implementation. */
public final class EmptyJInternalJob implements JInternalJob
{
    public JOrderProcess toOrderProcess(Step step) {
        return JOrderProcess.of(
            CompletableFuture.supplyAsync(
                () -> JOutcome.succeeded()));
    }
}
