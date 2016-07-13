package com.sos.scheduler.engine.data.job;

import com.sos.scheduler.engine.base.sprayjson.JavaEnumJsonFormat;
import spray.json.JsonFormat;

public enum JobState {
    // Dieselben Codes wie in spooler_job.h Job::State

    not_initialized,
    initialized,
    loaded,
    stopping,             // Wird gestoppt (Zustand, solange noch Tasks laufen, danach s_stopped). Sollte durch _is_stopped ersetzt werden!
    stopped,              // Gestoppt (z.B. wegen Fehler). Keine Task wird gestartet.
    error,                // Ein Fehler ist aufgetreten (nicht vom Skript), der Job ist nicht mehr aufrufbar.
    pending,              // Warten auf Start
    running;              // Mindestens eine Task läuft (die Tasks können aber ausgesetzt, also gerade nicht aktiv sein: s_suspended etc.)

    public static final JsonFormat<JobState> MyJsonFormat = new JavaEnumJsonFormat<>(JobState.class);
}
