package com.sos.scheduler.engine.data.filebased;

import com.sos.scheduler.engine.data.base.JavaEnumJsonFormat;
import spray.json.JsonFormat;

public enum FileBasedState {
    /** Fehler in XML-Definition */
    undefined("undefined"),

    /** on_initialized() gescheitert, Objekt ist nicht im Folder. */
    notInitialized("not_initialized"),

    /** on_initialized() ok, Objekt sieht gut aus. */
    initialized("initialized"),

    /** Mit Daten gefüllt: bei Jobs die Task-Warteschlange, bei Jobketten die Auftragswarteschlangen */
    loaded("loaded"),

    /** Requisite fehlt (beim Versuch zu aktivieren)- */
    incomplete("incomplete"),

    /** */
    active("active"),

    /** */
    closed("closed");


    private final String cppName;

    FileBasedState(String cppName) {
        this.cppName = cppName;
    }

    public static FileBasedState ofCppName(String name) {
        for (FileBasedState s: values())
            if (s.cppName.equals(name)) return s;
        throw new IllegalArgumentException(name);
    }

    public static final JsonFormat<FileBasedState> MyJsonFormat = new JavaEnumJsonFormat<>(FileBasedState.class);
}
