package com.sos.scheduler.engine.jobapi.scripting;

import com.google.common.base.Supplier;
import com.google.common.collect.ImmutableMap;

public class JobScriptInstanceAdapter {
    private final ScriptInstance scriptInstance;

    public JobScriptInstanceAdapter(String language, Supplier<ImmutableMap<String,Object>> bindingsLazy, String script) {
        this.scriptInstance = new ScriptInstance(language, bindingsLazy, script);
    }

    public final boolean callInit(boolean deflt) throws Exception {
        loadScript();
        return scriptInstance.callBooleanWhenExists(deflt, "spooler_init");
    }

    public final void callExit() {
        try {
            scriptInstance.callWhenExists("spooler_exit");
        } finally {
            scriptInstance.close();
        }
    }

    public final boolean callOpen(boolean deflt) throws Exception {
        return scriptInstance.callBooleanWhenExists(deflt, "spooler_open");
    }

    public final void callClose() throws Exception {
        scriptInstance.callWhenExists("spooler_close");
    }

    public final boolean callProcess(boolean deflt) throws Exception {
        return scriptInstance.callBooleanWhenExists(deflt, "spooler_process");
    }

    public final void callOnError() throws Exception {
        scriptInstance.callWhenExists("spooler_on_error");
    }

    public final void callOnSuccess() throws Exception {
        scriptInstance.callWhenExists("spooler_on_success");
    }

    public final boolean callTaskBefore() throws Exception {
        loadScript();
        return scriptInstance.callBooleanWhenExists(true, "spooler_task_before");
    }

    public final void callTaskAfter() throws Exception {
        try {
            scriptInstance.callWhenExists("spooler_task_after");
        } finally {
            scriptInstance.close();
        }
    }

    public final boolean callProcessBefore() throws Exception {
        return scriptInstance.callBooleanWhenExists(true, "spooler_process_before");
    }

    public final boolean callProcessAfter(boolean spoolerProcessResult) throws Exception {
        return scriptInstance.callBooleanWhenExists(spoolerProcessResult, "spooler_process_after", spoolerProcessResult);
    }

    private void loadScript() {
        scriptInstance.loadScript();
    }
}
