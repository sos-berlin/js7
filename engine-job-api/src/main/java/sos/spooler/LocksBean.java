package sos.spooler;

import static sos.spooler.Beans.toDelegate;

public final class LocksBean implements Bean<Locks>{
    private final Locks delegate;

    LocksBean(Locks delegate) {
        this.delegate = delegate;
    }

    public LockBean lock(String path) {
        return delegate.lock(path).toBean();
    }

    public LockBean lock_or_null(String path) {
        return delegate.lock_or_null(path).toBean();
    }

    public LockBean create_lock() {
        return delegate.create_lock().toBean();
    }

    public void add_lock(LockBean lock) {
        delegate.add_lock(toDelegate(lock));
    }

    @Override public Locks getDelegate() {
        return delegate;
    }
}
