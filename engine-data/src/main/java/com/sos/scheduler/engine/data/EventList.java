package com.sos.scheduler.engine.data;

import com.google.common.collect.ImmutableList;
import com.sos.scheduler.engine.data.event.Event;
import com.sos.scheduler.engine.data.filebased.FileBasedActivated;
import com.sos.scheduler.engine.data.filebased.FileBasedRemoved;
import com.sos.scheduler.engine.data.job.TaskEnded;
import com.sos.scheduler.engine.data.job.TaskStarted;
import com.sos.scheduler.engine.data.order.OrderFinished;
import com.sos.scheduler.engine.data.order.OrderResumed;
import com.sos.scheduler.engine.data.order.OrderSetBack;
import com.sos.scheduler.engine.data.order.OrderNodeChanged;
import com.sos.scheduler.engine.data.order.OrderStepEnded;
import com.sos.scheduler.engine.data.order.OrderStepStarted;
import com.sos.scheduler.engine.data.order.OrderSuspended;
import com.sos.scheduler.engine.data.order.OrderStarted;

public final class EventList {
    @SuppressWarnings("varargs")
    private static final ImmutableList<Class<? extends Event>> eventClassList = ImmutableList.<Class<? extends Event>>of(
            FileBasedActivated.class,
            FileBasedRemoved.class,
            TaskStarted.class,
            TaskEnded.class,
            OrderStarted.class,
            OrderFinished.class,
            OrderSuspended.class,
            OrderResumed.class,
            OrderStepStarted.class,
            OrderStepEnded.class,
            OrderNodeChanged.class,
            OrderSetBack.class);

    public static Class<?>[] eventClassArray() {
        return eventClassList.toArray(new Class<?>[0]);
    }

    private EventList() {}
}
