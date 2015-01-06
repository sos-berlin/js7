package com.sos.scheduler.engine.data.log;

/** Eine Meldung im Hauptprotokoll des Gewichts "error". */
public class WarningLogEvent extends LogEvent {
    public WarningLogEvent(String message) {
        super(SchedulerLogLevel.warning, message);
    }
}
