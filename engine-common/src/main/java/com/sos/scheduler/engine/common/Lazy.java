package com.sos.scheduler.engine.common;

/** Thread-sichere verzögerte Berechnung (oder Initialisierung) eines Werts. */
public abstract class Lazy<T> {
    private volatile boolean computed = false;
    private volatile T value = null;

    public final T get() {
        if (!computed) {
            synchronized (this) {
                if (!computed) {
                    value = compute();
                    computed = true;
                }
            }
        }
        return value;
    }

    public final boolean isDefined() {
        return computed;
    }

    /** Die Methode wird synchronized aufgerufen, also bei der Implementierung auf Deadlock achten! */
    protected abstract T compute();
}
