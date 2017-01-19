package com.sos.scheduler.engine.base.process;

import com.sos.scheduler.engine.base.sprayjson.JavaEnumJsonFormat;
import spray.json.JsonFormat;

/**
 * See Unix `kill` command.
 *
 * @author Joacim Zschimmer
 */
public enum ProcessSignal {
    SIGTERM(15),
    SIGKILL(9);

    private final int value;

    ProcessSignal(int value) {
        this.value = value;
    }

    public int value() {
        return value;
    }

    public static final JsonFormat<ProcessSignal> MyJsonFormat = new JavaEnumJsonFormat<>(ProcessSignal.class);
}
