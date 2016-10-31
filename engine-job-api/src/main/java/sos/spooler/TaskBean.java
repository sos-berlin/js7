package sos.spooler;

import static sos.spooler.Beans.toBean;

/** Ein Proxy von {@link Task}, mit Gettern und Settern für Skriptsprachen. */
public final class TaskBean implements Bean<Task> {
    private final Task delegate;

    TaskBean(Task task) {
        delegate = task;
    }

    public JobBean getJob() {
        return toBean(delegate.job());
    }

    public int getId() {
        return delegate.id();
    }

    public Variable_setBean getParams() {
        return toBean(delegate.params());
    }

    public void setResult(String o) {
        delegate.set_result(o);
    }

    public String getResult() {
        return delegate.result();
    }

    public void setRepeat(double seconds) {
        delegate.set_repeat(seconds);
    }

    public void end() {
        delegate.end();
    }

    public void setError(String o) {
        delegate.set_error(o);
    }

    public ErrorBean getError() {
        return toBean(delegate.error());
    }

    public boolean wait_until_terminated() {
        return delegate.wait_until_terminated();
    }

    public boolean wait_until_terminated(double waitSeconds) {
        return delegate.wait_until_terminated(waitSeconds);
    }

    public void set_history_field(String name, String value) {
        delegate.set_history_field(name, value);
    }

    public void setDelay_spooler_process(double seconds) {
        delegate.set_delay_spooler_process(seconds);
    }

    public void setDelay_spooler_process(String hhmmss) {
        delegate.set_delay_spooler_process(hhmmss);
    }

    public OrderBean getOrder() {
        return toBean(delegate.order());
    }

    public String getChanged_directories() {
        return delegate.changed_directories();
    }

    public void add_pid(int pid) {
        delegate.add_pid(pid);
    }

    public void add_pid(int pid, double timeoutSeconds) {
        delegate.add_pid(pid, timeoutSeconds);
    }

    public void remove_pid(int pid) {
        delegate.remove_pid(pid);
    }

    public String getStdout_text() {
        return delegate.stdout_text();
    }

    public String getStdout_path() {
        return delegate.stdout_path();
    }

    public String getStderr_text() {
        return delegate.stderr_text();
    }

    public String getStderr_path() {
        return delegate.stderr_path();
    }

    public SubprocessBean create_subprocess() {
        return toBean(delegate.create_subprocess());
    }

    public SubprocessBean create_subprocess(String commandLine) {
        return toBean(delegate.create_subprocess(commandLine));
    }

    public SubprocessBean create_subprocess(String[] filenameAndArguments) {
        return toBean(delegate.create_subprocess(filenameAndArguments));
    }

    public Web_serviceBean getWeb_service() {
        return toBean(delegate.web_service());
    }

    public Web_serviceBean getWeb_service_or_null() {
        return toBean(delegate.web_service_or_null());
    }

    public void setPriority(int o) {
        delegate.set_priority(o);
    }

    public int getPriority() {
        return delegate.priority();
    }

    public void setPriority_class(String o) {
        delegate.set_priority_class(o);
    }

    public String getPriority_class() {
        return delegate.priority_class();
    }

    public void setExit_code(int o) {
        delegate.set_exit_code(o);
    }

    public int getExit_code() {
        return delegate.exit_code();
    }

    public String getTrigger_files() {
        return delegate.trigger_files();
    }

    public boolean try_hold_lock(String lockPath) {
        return delegate.try_hold_lock(lockPath);
    }

    public boolean try_hold_lock_non_exclusive(String lockPath) {
        return delegate.try_hold_lock_non_exclusive(lockPath);
    }

    public void call_me_again_when_locks_available() {
        delegate.call_me_again_when_locks_available();
    }

    public String getWeb_service_access_token() {
        return delegate.web_service_access_token();
    }

    @Override public Task getDelegate() {
        return delegate;
    }
}
