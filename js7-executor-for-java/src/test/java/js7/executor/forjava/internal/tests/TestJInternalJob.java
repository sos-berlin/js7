package js7.executor.forjava.internal.tests;

import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionStage;
import java.util.concurrent.Executor;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import js7.data.value.NumberValue;
import js7.executor.forjava.internal.JInternalJob;
import js7.executor.forjava.internal.JOrderProcess;
import js7.executor.forjava.internal.JOrderResult;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import static java.util.Collections.singletonMap;
import static java.util.concurrent.ForkJoinPool.commonPool;
import static java.util.concurrent.TimeUnit.MILLISECONDS;

// For a simpler example, see TestBlockingInternalJob
public final class TestJInternalJob implements JInternalJob
{
    private static final Logger logger = LoggerFactory.getLogger(TestBlockingInternalJob.class);
    private static final int delayMillis = 500;

    private final JJobContext jobContext;
    private volatile String started = "NOT STARTED";

    // Schedule like Java 9:
    private final ScheduledExecutorService scheduler = new ScheduledThreadPoolExecutor(0);
    private Executor delayedExecutor(long delay, TimeUnit unit) {
      return runnable -> scheduler.schedule(() -> commonPool().execute(runnable), delay, unit);
    }

    public TestJInternalJob(JJobContext jobContext) {
        this.jobContext = jobContext;
    }

    // Not yet possible
    //public void stop() {
    //    scheduler.shutdown();
    //}

    public CompletionStage<Void> start() {
        return CompletableFuture.runAsync(
            () -> started = "STARTED");
    }

    public JOrderProcess processOrder(JOrderContext context) {
        return JOrderProcess.of(
            CompletableFuture
                .supplyAsync(
                    () -> process(context),
                    delayedExecutor(delayMillis, MILLISECONDS))
                .thenCombine(context.sendOut("TEST FOR OUT\n"), (a, b) -> a)
                .thenCombine(context.sendOut("FROM " + TestJInternalJob.class.getName() + "\n"), (a, b) -> a)
                .thenCombine(context.sendErr("TEST FOR ERR"), (a, b) -> a));
    }

    private JOrderResult process(JOrderContext context) {
        logger.debug("processOrder " + context.order().id());
        // JS7 guarantees having awaited completion of the `start` method
        if (!started.equals("STARTED")) {
            throw new RuntimeException("NOT STARTED");
        }
        // 💥 May throw NullPointerException or ArithmeticException 💥
        long arg = ((NumberValue)context.arguments().get("arg")).toBigDecimal().longValueExact();
        long result = arg + 1;
        return JOrderResult.of(
            singletonMap("RESULT", NumberValue.of(result)));
    }
}
