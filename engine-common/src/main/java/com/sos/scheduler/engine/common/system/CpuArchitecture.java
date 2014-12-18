package com.sos.scheduler.engine.common.system;

public enum CpuArchitecture {
    x86 ("x86", "Win32"),
    x64 ("x64", "x64");

    private final String officialName;
    private final String visualStudioName;

    CpuArchitecture(String officialName, String visualStudioName) {
        this.officialName = officialName;
        this.visualStudioName = visualStudioName;
    }

    public static CpuArchitecture cpuArchitecture() {
        return System.getProperty("os.arch").equals("amd64")? x64 : x86;
    }

    public final String officialName() {
        return officialName;
    }

    public final String visualStudioName() {
        return visualStudioName;
    }
}
