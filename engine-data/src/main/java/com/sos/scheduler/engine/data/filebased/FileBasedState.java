package com.sos.scheduler.engine.data.filebased;

import com.sos.scheduler.engine.base.sprayjson.JavaEnumJsonFormat;
import spray.json.JsonFormat;

public enum FileBasedState {
    // Same order as FileBaseState in Java !

    /** Fehler in XML-Definition */
    undefined,

    /** on_initialized() gescheitert, Objekt ist nicht im Folder. */
    not_initialized,

    /** on_initialized() ok, Objekt sieht gut aus. */
    initialized,

    /** Mit Daten gefüllt: bei Jobs die Task-Warteschlange, bei Jobketten die Auftragswarteschlangen */
    loaded,

    /** Requisite fehlt (beim Versuch zu aktivieren)- */
    incomplete,

    active,

    closed;

    public static FileBasedState ofCppName(String name) {
        return valueOf(name);
    }

    public static final JsonFormat<FileBasedState> MyJsonFormat = new JavaEnumJsonFormat<>(FileBasedState.class);

    /** Experimental. */
    public boolean isOkay() {
        return this == not_initialized ||  // For non file-based "ad-hoc" orders
            this == active;
    }
}
