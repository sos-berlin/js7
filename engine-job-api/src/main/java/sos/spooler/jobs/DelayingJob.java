package sos.spooler.jobs;

import java.time.Duration;
import java.time.format.DateTimeParseException;
import sos.spooler.Job_impl;

public final class DelayingJob extends Job_impl {

    private Duration delay = Duration.ofSeconds(1);

    @Override
    public boolean spooler_init() {
        String delayString = spooler_task.params().value("delay");
        if (!delayString.isEmpty()) {
            try {
                delay = Duration.parse(delayString);
            } catch (DateTimeParseException ignored) {
                try {
                    delay = Duration.parse("PT" + delayString);
                } catch (DateTimeParseException ignored2) {}
            }
        }
        return true;
    }

    @Override
    public boolean spooler_process() throws InterruptedException {
        Thread.sleep(delay.toMillis());
        return true;
    }
}
