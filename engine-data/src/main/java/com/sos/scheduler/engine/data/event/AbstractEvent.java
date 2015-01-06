package com.sos.scheduler.engine.data.event;

public abstract class AbstractEvent implements Event {
    @Override public String toString() {
        return getClass().getSimpleName();
    }
}
