package js7.launcher.forjava.internal.tests;

import js7.data_for_java.order.JOutcome;
import js7.launcher.forjava.internal.BlockingInternalJob;

public class InterruptibleBlockingInternalJob implements BlockingInternalJob {

    public OrderProcess toOrderProcess(Step step) {
        return new InterruptibleOrderProcess() {
            public JOutcome.Completed runInterruptible() throws InterruptedException {
                Thread thisIsMyThread = thread();
                step.out().println("InterruptibleBlockingInternalJob");
                Thread.sleep(Integer.MAX_VALUE);
                return JOutcome.succeeded();
            }
        };
    }
}
