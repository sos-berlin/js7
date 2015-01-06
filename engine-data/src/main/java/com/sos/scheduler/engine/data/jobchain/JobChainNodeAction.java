package com.sos.scheduler.engine.data.jobchain;

import com.sos.scheduler.engine.data.base.JavaEnumJsonFormat;
import spray.json.JsonFormat;

public enum JobChainNodeAction {
    process("process"),
    stop("stop"),
    nextState("next_state");

    private final String cppName;

    JobChainNodeAction(String cppName) {
        this.cppName = cppName;
    }

    public final String toCppName() {
        return cppName;
    }

    public static JobChainNodeAction ofCppName(String cppName) {
        for (JobChainNodeAction a: values()) {
            if (a.cppName.equals(cppName))
                return a;
        }
        throw new RuntimeException("Invalid JobChainNodeAction: "+cppName);
    }

    public static final JsonFormat<JobChainNodeAction> MyJsonFormat = new JavaEnumJsonFormat<>(JobChainNodeAction.class);
}
