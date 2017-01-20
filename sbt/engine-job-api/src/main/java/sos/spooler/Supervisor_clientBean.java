package sos.spooler;

public final class Supervisor_clientBean implements Bean<Supervisor_client> {
    private final Supervisor_client delegate;

    Supervisor_clientBean(Supervisor_client delegate) {
        this.delegate = delegate;
    }

    public String getHostname() {
        return delegate.hostname();
    }

    public int getTcp_port() {
        return delegate.tcp_port();
    }

    @Override public Supervisor_client getDelegate() {
        return delegate;
    }
}
