package js7.executor.forjava.internal.tests;

import java.util.concurrent.CompletableFuture;
import js7.data.value.NumberValue;
import js7.data.value.Value;
import js7.executor.forjava.internal.BlockingInternalJob;
import js7.executor.forjava.internal.JOrderResult;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import static com.google.common.base.Throwables.throwIfUnchecked;
import static java.lang.Thread.currentThread;
import static java.util.Collections.singletonMap;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.MatcherAssert.assertThat;

public final class TestBlockingInternalJob implements BlockingInternalJob
{
    private static final Logger logger = LoggerFactory.getLogger(TestBlockingInternalJob.class);
    String blockingThreadPrefix;

    public TestBlockingInternalJob(JJobContext jobContext) {
        blockingThreadPrefix = jobContext.jobArguments().get("expectedThreadPrefix").convertToString();
        assertSpecialThread();
    }

    public JOrderResult processOrder(JOrderContext context) throws Exception {
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

        Value maybeValue = context.arguments().get("arg");  // May be null
        // 💥 May throw NullPointerException or ArithmeticException 💥
        long arg = ((NumberValue)maybeValue).toBigDecimal().longValueExact();
        long result = arg + 1;
        return JOrderResult.of(
            singletonMap("RESULT", NumberValue.of(result)));
    }

    private void assertSpecialThread() {
        // Test only: this blocking call runs in a JS7 I/O thread
        assertThat("thread=" + currentThread().getName() + ", but expectedThreadPrefix=" + blockingThreadPrefix,
            currentThread().getName().startsWith(blockingThreadPrefix),
            equalTo(true));
    }

    private void doSomethingInParallel() {
        try {
            CompletableFuture<String> future = CompletableFuture.supplyAsync(() -> currentThread().getName());
            assert !future.get().startsWith(blockingThreadPrefix);
        } catch (Throwable t) {
            throwIfUnchecked(t);
            throw new RuntimeException(t);
        }
    }
}
