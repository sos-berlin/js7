package com.sos.scheduler.engine.data;

import com.google.common.collect.ImmutableList;
import com.sos.scheduler.engine.data.event.Event;
import com.sos.scheduler.engine.data.filebased.FileBasedActivatedEvent;
import com.sos.scheduler.engine.data.filebased.FileBasedRemovedEvent;
import com.sos.scheduler.engine.data.job.TaskEndedEvent;
import com.sos.scheduler.engine.data.job.TaskStartedEvent;
import com.sos.scheduler.engine.data.order.OrderFinishedEvent;
import com.sos.scheduler.engine.data.order.OrderResumedEvent;
import com.sos.scheduler.engine.data.order.OrderSetBackEvent;
import com.sos.scheduler.engine.data.order.OrderStateChangedEvent;
import com.sos.scheduler.engine.data.order.OrderStepEndedEvent;
import com.sos.scheduler.engine.data.order.OrderStepStartedEvent;
import com.sos.scheduler.engine.data.order.OrderSuspendedEvent;
import com.sos.scheduler.engine.data.order.OrderTouchedEvent;

public final class EventList {
    @SuppressWarnings("varargs")
    private static final ImmutableList<Class<? extends Event>> eventClassList = ImmutableList.<Class<? extends Event>>of(
            FileBasedActivatedEvent.class,
            FileBasedRemovedEvent.class,
            TaskStartedEvent.class,
            TaskEndedEvent.class,
            OrderTouchedEvent.class,
            OrderFinishedEvent.class,
            OrderSuspendedEvent.class,
            OrderResumedEvent.class,
            OrderStepStartedEvent.class,
            OrderStepEndedEvent.class,
            OrderStateChangedEvent.class,
            OrderSetBackEvent.class);

    public static Class<?>[] eventClassArray() {
        return eventClassList.toArray(new Class<?>[0]);
    }

    private EventList() {}
}
