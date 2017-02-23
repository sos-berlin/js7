package com.sos.scheduler.engine.taskserver.spoolerapi

import com.sos.scheduler.engine.minicom.idispatch.IDispatch
import org.scalatest.FreeSpec

/**
 * @author Joacim Zschimmer
 */
final class TypedNamedIDispatchesTest extends FreeSpec {

  "spoolerLog" in {
    val spoolerLog = new IDispatch.Empty {}
    val spoolerTask = new IDispatch.Empty {}
    val spoolerJob = new IDispatch.Empty {}
    val spooler = new IDispatch.Empty {}
    val namedIDispatches = TypedNamedIDispatches(List(
      "spooler_log" → spoolerLog,
      "spooler_task" → spoolerTask,
      "spooler_job" → spoolerJob,
      "spooler" → spooler))
    assert(namedIDispatches.toMap.size == 4)
  }

  "invalid name" in {
    intercept[IllegalArgumentException] { TypedNamedIDispatches(List("invalid" → new IDispatch.Empty {})) }
  }
}
