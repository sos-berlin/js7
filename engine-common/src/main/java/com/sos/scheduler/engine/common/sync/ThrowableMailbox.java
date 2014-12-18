package com.sos.scheduler.engine.common.sync;


import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.concurrent.atomic.AtomicReference;

import static com.google.common.base.MoreObjects.firstNonNull;
import static com.google.common.base.Throwables.propagate;

public class ThrowableMailbox<T extends Throwable> {
    private static final Logger logger = LoggerFactory.getLogger(ThrowableMailbox.class);
    
    private final AtomicReference<T> throwableAtom = new AtomicReference<T>();
    
    public final void setIfFirst(T o) {
        boolean isFirst = throwableAtom.compareAndSet(null, o);
        if (!isFirst  &&  logger.isErrorEnabled()) logger.error("Second exception ignored: " + o, o);
    }

    public final void throwUncheckedIfSet() {
        T throwable = fetch();
        if (throwable != null) throw propagate(throwable);
    }

    private T fetch() {
        return throwableAtom.getAndSet(null);
    }

    @Override public String toString() {
        return ThrowableMailbox.class.getSimpleName() +"("+ firstNonNull(throwableAtom.get(), "") +")";
    }
}
