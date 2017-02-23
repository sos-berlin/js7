package sos.spooler;

public final class SubprocessBean implements Bean<Subprocess> {
    private final Subprocess delegate;

    SubprocessBean(Subprocess delegate) {
        this.delegate = delegate;
    }

    public void close() {
        delegate.close();
    }

    public void start(String commandLine) {
        delegate.start(commandLine);
    }

    public void start(String filenameAndArguments[]) {
        delegate.start(filenameAndArguments);
    }

    public int getPid() {
        return delegate.pid();
    }

    public boolean isTerminated() {
        return delegate.terminated();
    }

    public int getExit_code() {
        return delegate.exit_code();
    }

    public int getTermination_signal() {
        return delegate.termination_signal();
    }

    public void setIgnore_error(boolean b) {
        delegate.set_ignore_error(b);
    }

    public boolean isIgnore_error() {
        return delegate.ignore_error();
    }

    public void setIgnore_signal(boolean b) {
        delegate.set_ignore_signal(b);
    }

    public boolean isIgnore_signal() {
        return delegate.ignore_signal();
    }

    public void setTimeout(double seconds) {
        delegate.set_timeout(seconds);
    }

    public void set_environment(String entryName, String value) {
        delegate.set_environment(entryName, value);
    }

    public boolean wait_for_termination(double seconds) {
        return delegate.wait_for_termination(seconds);
    }

    public void wait_for_termination() {
        delegate.wait_for_termination();
    }

    public void kill(int signal) {
        delegate.kill(signal);
    }

    public void kill() {
        delegate.kill();
    }

    public void setPriority(int priority) {
        delegate.set_priority(priority);
    }

    public int getPriority() {
        return delegate.priority();
    }

    public void setPriority_class(String priorityClass) {
        delegate.set_priority_class(priorityClass);
    }

    public String getPriority_class() {
        return delegate.priority_class();
    }

    public void setOwn_process_group(boolean b) {
        delegate.set_own_process_group(b);
    }

    public boolean isOwn_process_group() {
        return delegate.own_process_group();
    }

    public Variable_setBean getEnv() {
        return delegate.env().toBean();
    }

    @Override public Subprocess getDelegate() {
        return delegate;
    }
}
