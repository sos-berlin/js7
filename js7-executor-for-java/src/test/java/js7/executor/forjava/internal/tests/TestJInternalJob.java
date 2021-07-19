package js7.executor.forjava.internal.tests;

import io.vavr.control.Either;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionStage;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.Executor;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import js7.base.problem.Problem;
import js7.data.value.NumberValue;
import js7.data_for_java.order.JOutcome;
import js7.executor.forjava.internal.JInternalJob;
import js7.executor.forjava.internal.JOrderProcess;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import static io.vavr.control.Either.right;
import static java.lang.System.lineSeparator;
import static java.util.Collections.singletonMap;
import static java.util.concurrent.ForkJoinPool.commonPool;
import static java.util.concurrent.TimeUnit.MILLISECONDS;

// For a simpler example, see TestBlockingInternalJob
public final class TestJInternalJob implements JInternalJob
{
    // Public static for testing
    public static Map<String, Boolean> stoppedCalled = new ConcurrentHashMap<>();

    private static final Logger logger = LoggerFactory.getLogger(TestBlockingInternalJob.class);
    private static final int delayMillis = 500;

    private final String blockingThreadPoolName;
    private volatile String started = "NOT STARTED";

    // Schedule like Java 9:
    private final ScheduledExecutorService scheduler = new ScheduledThreadPoolExecutor(0);
    private Executor delayedExecutor(long delay, TimeUnit unit) {
      return runnable -> scheduler.schedule(() -> commonPool().execute(runnable), delay, unit);
    }

    public TestJInternalJob(JobContext jobContext) {
        blockingThreadPoolName = jobContext.jobArguments().get("blockingThreadPoolName").convertToString();
    }

    public CompletionStage<Either<Problem,Void>> start() {
        return CompletableFuture.supplyAsync(
            () -> {
                started = "STARTED";
                return right(null);
            });
    }

    public CompletionStage<Void> stop() {
        return CompletableFuture.supplyAsync(
            () -> {
                stoppedCalled.put(blockingThreadPoolName, true);
                scheduler.shutdown();
                return null;
            });
    }

    public JOrderProcess toOrderProcess(Step step) {
        return JOrderProcess.of(
            CompletableFuture
                .supplyAsync(
                    () -> process(step),
                    delayedExecutor(delayMillis, MILLISECONDS))
                .thenCombine(step.sendOut("TEST FOR OUT" + lineSeparator()), (a, b) -> a)
                .thenCombine(step.sendOut("FROM " + TestJInternalJob.class.getName() + lineSeparator()), (a, b) -> a)
                .thenCombine(step.sendErr("TEST FOR ERR" + lineSeparator()), (a, b) -> a));
    }

    private JOutcome.Completed process(Step step) {
        logger.debug("process " + step.order().id());
        // JS7 guarantees having awaited completion of the `start` method
        if (!started.equals("STARTED")) {
            throw new RuntimeException("NOT STARTED");
        }
        // 💥 May throw NullPointerException or ArithmeticException 💥
        long arg = ((NumberValue)step.arguments().get("STEP_ARG")).toBigDecimal().longValueExact();
        long result = arg + 1;
        return JOutcome.succeeded(
            singletonMap("RESULT", NumberValue.of(result)));
    }
}
