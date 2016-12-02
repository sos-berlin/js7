package com.sos.scheduler.engine.data.jobchain;

import com.sos.scheduler.engine.base.sprayjson.JavaEnumJsonFormat;
import spray.json.JsonFormat;

/**
 * @author Joacim Zschimmer
 */
public enum JobChainState {
    under_construction,
    initialized,
    loaded,
    running,
    stopped,
    closed;

    public static final JsonFormat<JobChainState> jsonFormat = new JavaEnumJsonFormat<>(JobChainState.class);
}
