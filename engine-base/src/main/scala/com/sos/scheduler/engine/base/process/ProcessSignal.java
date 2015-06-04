package com.sos.scheduler.engine.base.process;

import com.sos.scheduler.engine.base.sprayjson.JavaEnumJsonFormat;
import spray.json.JsonFormat;

/**
 * See Unix `kill` command.
 *
 * @author Joacim Zschimmer
 */
public enum ProcessSignal {
    SIGTERM,
    SIGKILL;

    public static final JsonFormat<ProcessSignal> MyJsonFormat = new JavaEnumJsonFormat<>(ProcessSignal.class);
}
