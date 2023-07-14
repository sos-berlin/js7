package js7.launcher.forjava.internal.tests;

import io.vavr.control.Either;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;
import js7.base.problem.Problem;
import js7.data.job.JobResourcePath;
import js7.data.value.NumberValue;
import js7.data.value.Value;
import js7.data_for_java.order.JOutcome;
import js7.data_for_java.value.JExpression;
import js7.launcher.forjava.internal.BlockingInternalJob;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import static io.vavr.control.Either.right;
import static java.lang.System.lineSeparator;
import static java.lang.Thread.currentThread;
import static java.util.Collections.singletonMap;
import static js7.data_for_java.vavr.VavrUtils.getOrThrow;
import static js7.data_for_java.vavr.VavrUtils.toVavr;
import static org.hamcrest.CoreMatchers.anyOf;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.MatcherAssert.assertThat;

public final class TestBlockingInternalJob implements BlockingInternalJob
{
    // Public static for testing
    public static Map<String, Boolean> stoppedCalled = new ConcurrentHashMap<>();

    private static final Logger logger = LoggerFactory.getLogger(TestBlockingInternalJob.class);

    private final String expectedBlockingThreadPoolName;
    private boolean startCalled = false;

    public TestBlockingInternalJob(JobContext jobContext) {
        assert jobContext.asScala().executable().script().equals("TEST SCRIPT");
        java.util.Map<String, js7.data.value.Value> a = jobContext.jobArguments();
        Value b = a.get("blockingThreadPoolName");
        expectedBlockingThreadPoolName = b.convertToString();
        //expectedBlockingThreadPoolName = jobContext.jobArguments().get("blockingThreadPoolName").convertToString();
        assertSpecialThread();
        String name = jobContext.jobKey().name();
        if (!name.equals("WORKFLOW~1:JOB")
            && !name.equals("WORKFLOW-9~17:0"))
            throw new AssertionError("Unexpected jobKey.name = " + name);
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

    public OrderProcess toOrderProcess(Step step) {
        return () -> {
            assertThat(startCalled, equalTo(true));

            logger.debug("toOrderProcess " + step.order().id());
            // Blocking is allowed here, because it is a BlockingInternalJob
            assertSpecialThread();
            Thread.sleep(500);
            doSomethingInParallel();

            Either<Problem,Value> checkedValue = step.jobResourceVariable(JobResourcePath.of("A"), "stringSetting");
            assertThat(step.instructionLabel().isPresent(), equalTo(false));
            assertThat(step.jobName(), anyOf(
                equalTo("JOB"),
                equalTo("WORKFLOW-9~17:0")/*inline job*/));

            assertThat(
                getOrThrow(
                    JExpression.parse("$js7JobName")
                        .flatMap(step::evalExpression)
                        .flatMap(o -> toVavr(o.toStringValue()))
                        .map(o -> o.string())),
                equalTo(step.jobName()));

            step.out().println("TEST FOR OUT");
            step.out().println("FROM " + getClass().getName());

            // Test many write()
            String string = "TEST FOR ERR";
            for (int i = 0; i < string.length(); i++) {
                step.errWriter().write(string.charAt(i));
            }
            step.errWriter().write(lineSeparator());

            // The recommened way is to access the declared job arguments:
            Map<String,Value> arguments = step.arguments();
            Value argOrNull = arguments.get("STEP_ARG");
            //Value argOrNull = step.arguments().get("STEP_ARG");

            // Access any (maybe undeclared) named values
            // like $ORDER_ARG in the expression language.
            // Returns a declared Order default value, too.
            // STEP_ARG is not accessible here.
            assertThat(step.namedValue("ORDER_ARG"), equalTo(Optional
                .ofNullable(argOrNull)
                .map(o -> right(o))));
            assertThat(step.namedValue("UNKNOWN"), equalTo(Optional.empty()));

            // 💥 May throw NullPointerException or ArithmeticException 💥
            long arg = ((NumberValue)argOrNull).toBigDecimal().longValueExact();
            long result = arg + 1;

            return JOutcome.succeeded(singletonMap("RESULT", NumberValue.of(result)));
        };
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
            throw new RuntimeException(t);
        }
    }
}
