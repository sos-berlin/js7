package com.sos.scheduler.engine.data.log;

import com.sos.scheduler.engine.base.sprayjson.JavaEnumJsonFormat;
import spray.json.JsonFormat;

public enum SchedulerLogLevel {
    none(-10, "none"),
    debug9(-9, "debug9"),
    debug8(-8, "debug8"),
    debug7(-7, "debug7"),
    debug6(-6, "debug6"),
    debug5(-5, "debug5"),
    debug4(-4, "debug4"),
    debug3(-3, "debug3"),
    debug2(-2, "debug2"),
    debug1(-1, "debug1"),
    info(0, "info"),
    warning(1, "warn"),
    error(2, "error");

    public static final SchedulerLogLevel Min = debug9;
    public static final SchedulerLogLevel Max = error;
    private final int cppNumber;
    private final String cppName;

    SchedulerLogLevel(int cppNumber, String cppName) {
        this.cppNumber = cppNumber;
        this.cppName = cppName;
    }

    public boolean isDebug() {
        return cppNumber >= -9 && cppNumber <= -1;
    }

    public String cppName() {
        return cppName;
    }

    public int cppNumber() {
        return cppNumber;
    }

    /**
     * When `SchedulerLogLevel` is used as minimum level.
     * @return true, when minimumLevel contains level
     */
    public boolean contains(SchedulerLogLevel level) {
        return level.cppNumber >= cppNumber;
    }

    public static SchedulerLogLevel ofCpp(int cppLogLevel) {
        return values()[cppLogLevel - none.cppNumber];
    }

    public static final JsonFormat<SchedulerLogLevel> MyJsonFormat = new JavaEnumJsonFormat<>(SchedulerLogLevel.class);
}
