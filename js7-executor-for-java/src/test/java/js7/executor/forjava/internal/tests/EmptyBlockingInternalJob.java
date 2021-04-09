package js7.executor.forjava.internal.tests;

import js7.data_for_java.order.JOutcome;
import js7.executor.forjava.internal.BlockingInternalJob;

public final class EmptyBlockingInternalJob implements BlockingInternalJob
{
    public JOutcome.Completed processOrder(JOrderContext context) {
        return JOutcome.succeeded();
    }
}
