package com.sos.scheduler.engine.data.log;

public enum SchedulerLogLevel {
    none(-10, "none"),
    debug9(-9, "debug9"),
    @Deprecated debug8(-8, "debug8"),
    @Deprecated debug7(-7, "debug7"),
    @Deprecated debug6(-6, "debug6"),
    @Deprecated debug5(-5, "debug5"),
    @Deprecated debug4(-4, "debug4"),
    debug3(-3, "debug3"),
    @Deprecated debug2(-2, "debug2"),
    debug1(-1, "debug1"),
    info(0, "info"),
    warning(1, "warn"),
    error(2, "error");

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

    public static SchedulerLogLevel ofCpp(int cppLogLevel) {
        for (SchedulerLogLevel o: values())
            if (o.cppNumber == cppLogLevel) return o;
        throw new RuntimeException("Unknown C++ log_level: "+cppLogLevel);
    }
}
