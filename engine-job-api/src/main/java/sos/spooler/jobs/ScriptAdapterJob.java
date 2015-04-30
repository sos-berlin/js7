package sos.spooler.jobs;

import com.google.common.collect.ImmutableMap;
import com.sos.scheduler.engine.jobapi.scripting.JobScriptInstanceAdapter;
import sos.spooler.Job_impl;

/** Only used by C++. */
public class ScriptAdapterJob extends Job_impl {

    private final JobScriptInstanceAdapter adapter;

    public ScriptAdapterJob(String language, String script) throws Exception {
        ScriptAdapterHelper.Parameters p = ScriptAdapterHelper.parseLanguageParameter(language);
        adapter = new JobScriptInstanceAdapter(
                p.language,
                () -> {
                    ImmutableMap.Builder<String, Object> result = ImmutableMap.builder();
                    if (spooler != null)
                        result.put("spooler", ScriptAdapterHelper.conditionalToBean(p.isUsingBean, spooler));
                    if (spooler_task != null)
                        result.put("spooler_task", ScriptAdapterHelper.conditionalToBean(p.isUsingBean, spooler_task));
                    if (spooler_job != null)
                        result.put("spooler_job", ScriptAdapterHelper.conditionalToBean(p.isUsingBean, spooler_job));
                    if (spooler_log != null)
                        result.put("spooler_log", ScriptAdapterHelper.conditionalToBean(p.isUsingBean, spooler_log));
                    return result.build();
                },
                script);
    }

    @Override public final boolean spooler_init() throws Exception {
        return adapter.callInit(super.spooler_init());
    }

    @Override public final void spooler_exit() {
        adapter.callExit();
    }

    @Override public final boolean spooler_open() throws Exception {
        return adapter.callOpen(super.spooler_open());
    }

    @Override public final void spooler_close() throws Exception {
        adapter.callClose();
    }

    @Override public final boolean spooler_process() throws Exception {
        boolean defaultResult = spooler_task.order() != null;
        return adapter.callProcess(defaultResult);
    }

    @Override public final void spooler_on_error() throws Exception {
        adapter.callOnError();
    }

    @Override public final void spooler_on_success() throws Exception {
        adapter.callOnSuccess();
    }

    public final boolean spooler_task_before() throws Exception {
        return adapter.callTaskBefore();
    }

    public final void spooler_task_after() throws Exception {
        adapter.callTaskAfter();
    }

    public final boolean spooler_process_before() throws Exception {
        return adapter.callProcessBefore();
    }

    public final boolean spooler_process_after(boolean spoolerProcessResult) throws Exception {
        return adapter.callProcessAfter(spoolerProcessResult);
    }


}
