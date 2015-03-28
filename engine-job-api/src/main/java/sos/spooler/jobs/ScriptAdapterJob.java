package sos.spooler.jobs;

import com.google.common.collect.ImmutableMap;
import com.sos.scheduler.engine.jobapi.scripting.JobScriptInstanceAdapter;
import java.util.Objects;
import sos.spooler.HasBean;
import sos.spooler.Job_impl;

import static sos.spooler.Beans.toBean;

/** Only used by C++. */
public class ScriptAdapterJob extends Job_impl {
    private static final String beanLanguagePrefix = "java";
    private static final String methodLanguagePrefix = "javax.script";

    private final JobScriptInstanceAdapter adapter;

    public ScriptAdapterJob(String language, String script) throws Exception {
        Parameters p = parseLanguageParameter(language);
        adapter = new JobScriptInstanceAdapter(
                p.language,
                () -> {
                    ImmutableMap.Builder<String, Object> result = ImmutableMap.builder();
                    if (spooler != null)
                        result.put("spooler", conditionalToBean(p.isUsingBean, spooler));
                    if (spooler_task != null)
                        result.put("spooler_task", conditionalToBean(p.isUsingBean, spooler_task));
                    if (spooler_job != null)
                        result.put("spooler_job", conditionalToBean(p.isUsingBean, spooler_job));
                    if (spooler_log != null)
                        result.put("spooler_log", conditionalToBean(p.isUsingBean, spooler_log));
                    return result.build();
                },
                script);
    }

    static Parameters parseLanguageParameter(String prefixedLanguageString) {
        String[] parts = prefixedLanguageString.split(":", 2);
        if (parts.length != 2) throw new RuntimeException("Invalid language='"+ prefixedLanguageString +"'");
        boolean isBeanCall = languagePrefixIsBeanCall(parts[0]);
        String language = parts[1];
        return new Parameters(language, isBeanCall);
    }

    private static boolean languagePrefixIsBeanCall(String prefix) {
        if (prefix.equals(beanLanguagePrefix))
            return true;
        else
        if (prefix.startsWith(methodLanguagePrefix))
            return false;
        else
            throw new RuntimeException("Invalid language prefix '"+ prefix+ "'. '"+ beanLanguagePrefix +"' or '"+ methodLanguagePrefix + "' expected");
    }

    @SuppressWarnings("unchecked")
    private static Object conditionalToBean(boolean isToBean, HasBean<?> o) {
        return isToBean? toBean(o) : o;
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

    static final class Parameters {
        private final String language;
        private final boolean isUsingBean;

        Parameters(String language, boolean isBeanCall) {
            this.language = language;
            this.isUsingBean = isBeanCall;
        }

        @Override public boolean equals(Object o) {
            return language.equals(((Parameters)o).language) && isUsingBean == ((Parameters)o).isUsingBean;
        }

        @Override public int hashCode() {
            // This is to please Lint.
            return Objects.hashCode(language) + Boolean.hashCode(isUsingBean);
        }
    }
}
