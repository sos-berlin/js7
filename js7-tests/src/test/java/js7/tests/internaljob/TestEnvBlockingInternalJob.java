package js7.tests.internaljob;

import js7.data_for_java.order.JOutcome;
import js7.launcher.forjava.internal.BlockingInternalJob;
import static io.vavr.control.Either.right;
import static java.util.Collections.singletonMap;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.MatcherAssert.assertThat;

public final class TestEnvBlockingInternalJob implements BlockingInternalJob
{
    public OrderProcess toOrderProcess(Step step) {
        return () -> {
            assertThat(step.env(), equalTo(right(singletonMap("ENV", "ENV-VALUE"))));
            return JOutcome.succeeded();
        };
    }
}
