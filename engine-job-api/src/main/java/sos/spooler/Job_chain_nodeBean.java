package sos.spooler;

import static sos.spooler.Beans.toBean;

public final class Job_chain_nodeBean implements Bean<Job_chain_node> {
    private final Job_chain_node delegate;

    Job_chain_nodeBean(Job_chain_node delegate) {
        this.delegate = delegate;
    }

    public String getState() {
        return delegate.state();
    }

    public Job_chain_nodeBean getNext_node() {
        return toBean(delegate.next_node());
    }

    public Job_chain_nodeBean getError_node() {
        return toBean(delegate.error_node());
    }

    public JobBean getJob() {
        return toBean(delegate.job());
    }

    public String getNext_state() {
        return delegate.next_state();
    }

    public String getError_state() {
        return delegate.error_state();
    }

    public void setAction(String action) {
        delegate.set_action(action);
    }

    public String getAction() {
        return delegate.action();
    }

    @Override public Job_chain_node getDelegate() {
        return delegate;
    }
}
