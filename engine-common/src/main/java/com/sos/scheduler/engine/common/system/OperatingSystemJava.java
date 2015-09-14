package com.sos.scheduler.engine.common.system;

/**
 * @author Joacim Zschimmer
 */
public class OperatingSystemJava {
    public static final boolean isUnix = OperatingSystem$.MODULE$.isUnix();
    public static final boolean isWindows = OperatingSystem$.MODULE$.isWindows();
    public static final CpuArchitecture cpuArchitecture = OperatingSystem$.MODULE$.cpuArchitecture();

    public static String makeModuleFilename(String name) {
        return OperatingSystem$.MODULE$.makeModuleFilename(name);
    }

    public static String makeExecutableFilename(String name) {
        return OperatingSystem$.MODULE$.makeExecutableFilename(name);
    }

    public static String getDynamicLibraryEnvironmentVariableName() {
        return OperatingSystem$.MODULE$.getDynamicLibraryEnvironmentVariableName();
    }
}
