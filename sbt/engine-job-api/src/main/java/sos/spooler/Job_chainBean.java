package sos.spooler;

import static sos.spooler.Beans.toBean;
import static sos.spooler.Beans.toDelegate;

public final class Job_chainBean implements Bean<Job_chain>{
    private final Job_chain delegate;

    Job_chainBean(Job_chain delegate) {
        this.delegate = delegate;
    }

    public void setName(String o) {
        delegate.set_name(o);
    }

    public String getName() {
        return delegate.name();
    }

    public void add_job(String jobname, String inputState, String outputState, String errorState) {
        delegate.add_job(jobname, inputState, outputState, errorState);
    }

    public void add_end_state(String state) {
        delegate.add_end_state(state);
    }

    public void add_order(OrderBean order) {
        delegate.add_order(toDelegate(order));
    }

    public void add_or_replace_order(OrderBean order) {
        delegate.add_or_replace_order(toDelegate(order));
    }

    public int getOrder_count() {
        return delegate.order_count();
    }

    public Job_chain_nodeBean node(String state) {
        return toBean(delegate.node(state));
    }

    public Order_queueBean order_queue(String state) {
        return toBean(delegate.order_queue(state));
    }

    public void remove() {
        delegate.remove();
    }

    public void setOrders_recoverable(boolean b) {
        delegate.set_orders_recoverable(b);
    }

    public boolean getOrders_recoverable() {
        return delegate.orders_recoverable();
    }

    public void setTitle(String title) {
        delegate.set_title(title);
    }

    public String getTitle() {
        return delegate.title();
    }

    public String getPath() {
        return delegate.path();
    }

    public String[] getStates() {
        return delegate.states();
    }

    @Override public Job_chain getDelegate() {
        return delegate;
    }
}
