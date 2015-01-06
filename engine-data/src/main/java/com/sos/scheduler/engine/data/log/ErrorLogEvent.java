package com.sos.scheduler.engine.data.log;

/** Eine Meldung im Hauptprotokoll des Gewichts "error". */
public class ErrorLogEvent extends LogEvent {
    public ErrorLogEvent(String message) {
        super(SchedulerLogLevel.error, message);
    }
}
