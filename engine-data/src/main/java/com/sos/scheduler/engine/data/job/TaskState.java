package com.sos.scheduler.engine.data.job;

import com.sos.scheduler.engine.base.sprayjson.JavaEnumJsonFormat;
import spray.json.JsonFormat;

/**
 * Mirror of spooler_task.cxx, Task::State.
 * @author Joacim Zschimmer
 */
public enum TaskState {
    none("none"),
    loading("loading"),
    waiting_for_process("waiting_for_process"),
    starting("starting"),
    opening("opening"),
    opening_waiting_for_locks("opening_waiting_for_locks"),
    running("running"),
    running_delayed("running_delayed"),
    running_waiting_for_locks("running_waiting_for_locks"),
    running_waiting_for_order("running_waiting_for_order"),
    running_process("running_process"),
    running_remote_process("running_remote_process"),
    suspended("suspended"),
    ending("ending"),
    ending_waiting_for_subprocesses("ending_waiting_for_subprocesses"),
    on_success("on_success"),
    on_error("on_error"),
    exit("exit"),
    release("release"),
    killing("killing"),
    ended("ended"),
    deleting_files("deleting_files"),
    closed("closed");

    private final String name;

    TaskState(String name) {
        this.name = name;
    }

    public static TaskState of(String name) {
        for (TaskState o : values()) {
            if (o.name.equals(name))
                return o;
        }
        throw new IllegalArgumentException("Unknown TaskState '" + name + "'");
    }

    public static final JsonFormat<TaskState> MyJsonFormat = new JavaEnumJsonFormat<>(TaskState.class);
}
