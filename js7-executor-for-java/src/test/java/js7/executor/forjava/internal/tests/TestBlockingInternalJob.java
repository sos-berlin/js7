package js7.executor.forjava.internal.tests;

import io.vavr.control.Either;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;
import js7.base.problem.Problem;
import js7.data.value.NumberValue;
import js7.data.value.Value;
import js7.data.workflow.instructions.executable.WorkflowJob;
import js7.data_for_java.order.JOutcome;
import js7.executor.forjava.internal.BlockingInternalJob;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import static com.google.common.base.Throwables.throwIfUnchecked;
import static io.vavr.control.Either.right;
import static java.lang.Thread.currentThread;
import static java.util.Collections.singletonMap;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.MatcherAssert.assertThat;

public final class TestBlockingInternalJob implements BlockingInternalJob
{
    public static Map<String, Boolean> stoppedCalled = new ConcurrentHashMap<>();
    private static final Logger logger = LoggerFactory.getLogger(TestBlockingInternalJob.class);

    private final String expectedBlockingThreadPoolName;
    private boolean startCalled = false;

    public TestBlockingInternalJob(JJobContext jobContext) {
        expectedBlockingThreadPoolName = jobContext.jobArguments().get("blockingThreadPoolName").convertToString();
        assertSpecialThread();
    }

    // Implement optionally
    @Override public Either<Problem, Void> start() {
        assertThat(startCalled, equalTo(false));
        startCalled = true;
        return right(null);
    }

    // Implement optionally
    @Override public void stop() {
        stoppedCalled.put(expectedBlockingThreadPoolName, true);
    }

    public JOutcome.Completed processOrder(JOrderContext context) throws Exception {
        assertThat(startCalled, equalTo(true));

        Either<Problem,WorkflowJob.Name> maybeJobName =
            context.workflow().checkedJobName(context.order().workflowPosition().position());

        logger.debug("processOrder " + context.order().id());
        // Blocking is allowed here, because it is a BlockingInternalJob
        assertSpecialThread();
        Thread.sleep(500);
        doSomethingInParallel();

        context.out().println("TEST FOR OUT");
        context.out().println("FROM " + getClass().getName());

        // Test many write()
        String string = "TEST FOR ERR";
        for (int i = 0; i < string.length(); i++) {
            context.errWriter().write(string.charAt(i));
        }
        context.errWriter().write('\n');

        Value maybeValue = context.arguments().get("arg");  // May be null
        // 💥 May throw NullPointerException or ArithmeticException 💥
        long arg = ((NumberValue)maybeValue).toBigDecimal().longValueExact();
        long result = arg + 1;

        return JOutcome.succeeded(singletonMap("RESULT", NumberValue.of(result)));
    }

    private void assertSpecialThread() {
        // Test only: this blocking call runs in a JS7 I/O thread
        assertThat("thread=" + currentThread().getName() + ", but expectedThreadPoolName=" + expectedBlockingThreadPoolName,
            currentThread().getName().startsWith(expectedBlockingThreadPoolName),
            equalTo(true));
    }

    private void doSomethingInParallel() {
        try {
            CompletableFuture<String> future = CompletableFuture.supplyAsync(() -> currentThread().getName());
            assert !future.get().startsWith(expectedBlockingThreadPoolName);
        } catch (Throwable t) {
            throwIfUnchecked(t);
            throw new RuntimeException(t);
        }
    }
}
