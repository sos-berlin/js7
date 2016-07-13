package com.sos.scheduler.engine.data.configuration;

public final class SchedulerDataConstants {
    private SchedulerDataConstants() {}

    public static final long EternalCppMillis = Integer.MAX_VALUE * 1000L;     // Wie C++ time::eternal
    public static final long NeverCppMillis = Integer.MAX_VALUE * 1000L;     // Wie C++ Duration::never
}
