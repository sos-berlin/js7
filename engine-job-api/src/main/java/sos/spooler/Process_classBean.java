package sos.spooler;

public final class Process_classBean implements Bean<Process_class> {
    private final Process_class delegate;

    Process_classBean(Process_class delegate) {
        this.delegate = delegate;
    }

    public void setName(String name) {
        delegate.set_name(name);
    }

    public String getName() {
        return delegate.name();
    }

    public void setRemote_scheduler(String hostAndPort) {
        delegate.set_remote_scheduler(hostAndPort);
    }

    public String getRemote_scheduler() {
        return delegate.remote_scheduler();
    }

    public void setMax_processes(int n) {
        delegate.set_max_processes(n);
    }

    public int getMax_processes() {
        return delegate.max_processes();
    }

    public void remove() {
        delegate.remove();
    }

    @Override public Process_class getDelegate() {
        return delegate;
    }
}
