package com.sos.scheduler.engine.data.log;

/** Eine Meldung im Hauptprotokoll des Gewichts "error". */
public class InfoLogEvent extends LogEvent {
    public InfoLogEvent(String message) {
        super(SchedulerLogLevel.info, message);
    }
}
