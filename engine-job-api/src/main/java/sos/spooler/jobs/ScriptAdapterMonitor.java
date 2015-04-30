package sos.spooler.jobs;

import com.google.common.collect.ImmutableMap;
import com.sos.scheduler.engine.jobapi.scripting.JobScriptInstanceAdapter;
import sos.spooler.Monitor_impl;

/**
 * @author Andreas Liebert
 */
public class ScriptAdapterMonitor extends Monitor_impl {

    private final JobScriptInstanceAdapter adapter;

    public ScriptAdapterMonitor(String language, String script) throws Exception {
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

    @Override public final boolean spooler_task_before() throws Exception {
        return adapter.callTaskBefore();
    }

    @Override public final void spooler_task_after() throws Exception {
        adapter.callTaskAfter();
    }

    @Override public final boolean spooler_process_before() throws Exception {
        return adapter.callProcessBefore();
    }

    @Override public final boolean spooler_process_after(boolean spoolerProcessResult) throws Exception {
        return adapter.callProcessAfter(spoolerProcessResult);
    }
}
