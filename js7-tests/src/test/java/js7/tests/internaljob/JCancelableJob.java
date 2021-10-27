package js7.tests.internaljob;

import js7.data_for_java.order.JOutcome;
import js7.launcher.forjava.internal.BlockingInternalJob;

public final class JCancelableJob implements BlockingInternalJob
{
    public OrderProcess toOrderProcess(Step step)
    {
        return new OrderProcess() {
            volatile boolean canceled = false;

            public JOutcome.Completed run() throws InterruptedException {
                while (!canceled) {
                    Thread.sleep(10);
                }
                return JOutcome.failed("Canceled");
            }

            @Override
            public void cancel(boolean immediately) {
                canceled = true;
            }
        };
    }
}
