package js7.executor.forjava.internal.tests;

import js7.executor.forjava.internal.BlockingInternalJob;
import js7.executor.forjava.internal.JOrderResult;
import static java.util.Collections.emptyMap;

public final class EmptyBlockingInternalJob implements BlockingInternalJob
{
    public JOrderResult processOrder(JOrderContext context) {
        return JOrderResult.of(emptyMap());
    }
}
