package js7.launcher.forjava.internal.tests;

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
import js7.data.value.Value;
import js7.data_for_java.order.JOutcome;
import js7.launcher.forjava.internal.JInternalJob;
import js7.launcher.forjava.internal.JOrderProcess;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import static io.vavr.control.Either.right;
import static java.lang.System.lineSeparator;
import static java.util.Collections.singletonMap;
import static java.util.concurrent.TimeUnit.MILLISECONDS;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.MatcherAssert.assertThat;

// For a simpler example, see TestBlockingInternalJob
public final class TestJInternalJob implements JInternalJob
{
    // Public static for testing
    public static Map<String, Boolean> stoppedCalled = new ConcurrentHashMap<>();

    private JobContext jobContext;
    private static final Logger logger = LoggerFactory.getLogger(TestBlockingInternalJob.class);
    private static final int delayMillis = 500;

    private final String blockingThreadPoolName;
    private volatile String started = "NOT STARTED";

    // Schedule like Java 9:
    private final ScheduledExecutorService scheduler = new ScheduledThreadPoolExecutor(0);
    private Executor delayedExecutor(long delay, TimeUnit unit) {
      return runnable ->
          scheduler.schedule(() ->
              // Use internalJs7Executor only if you know what you do! DO NEVER BLOCK!
              // Otherwise, use ForkJoinPool.commonPool() or any other Executor.
              jobContext.internalJs7Executor().execute(runnable), delay, unit);
    }

    public TestJInternalJob(JobContext jobContext) {
        logger.debug("Constructor");
        this.jobContext = jobContext;
        Map<String,Value> arguments = jobContext.jobArguments();
        blockingThreadPoolName = arguments.get("blockingThreadNamePrefix").convertToString();
    }

    @Override
    public CompletionStage<Either<Problem,Void>> start() {
        return CompletableFuture.supplyAsync(
            () -> {
                logger.debug("start");
                started = "STARTED";
                return right(null);
            });
    }

    @Override
    public CompletionStage<Void> stop() {
        return CompletableFuture.supplyAsync(
            () -> {
                logger.debug("stop");
                assertThat(started, equalTo("STARTED"));
                stoppedCalled.put(blockingThreadPoolName, true);
                scheduler.shutdown();
                return null;
            });
    }

    public JOrderProcess toOrderProcess(Step step) {

        return JOrderProcess.of(CompletableFuture
            .supplyAsync(
                () -> null,
                delayedExecutor(delayMillis, MILLISECONDS))
            .thenApply(o -> {
                logger.debug("toOrderProcess");
                return o;
            })
            .thenCompose(o -> step
                .writeOut("TEST FOR OUT" + lineSeparator())
                .thenApply(x -> o))
            .thenCompose(o -> step
                .writeOut("FROM " + TestJInternalJob.class.getName() + lineSeparator())
                .thenApply(x -> o))
            .thenCompose(o -> step
                .writeErr("TEST FOR ERR" + lineSeparator())
                .thenApply(x -> o))
            .thenApply(o ->
                process(step)));
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
