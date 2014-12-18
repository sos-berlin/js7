package sos.spooler;

public final class LockBean implements Bean<Lock> {
    private final Lock delegate;

    LockBean(Lock delegate) {
        this.delegate = delegate;
    }

    public void setName(String name) {
        delegate.set_name(name);
    }

    public String getName() {
        return delegate.name();
    }

    public void setMax_non_exclusive(int n) {
        delegate.set_max_non_exclusive(n);
    }

    public int getMax_non_exclusive() {
        return delegate.max_non_exclusive();
    }

    public void remove() {
        delegate.remove();
    }

    @Override public Lock getDelegate() {
        return delegate;
    }
}
