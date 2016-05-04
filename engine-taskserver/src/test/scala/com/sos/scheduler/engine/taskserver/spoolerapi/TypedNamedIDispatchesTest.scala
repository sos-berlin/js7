package com.sos.scheduler.engine.taskserver.spoolerapi

import com.sos.scheduler.engine.minicom.idispatch.IDispatch
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class TypedNamedIDispatchesTest extends FreeSpec {

  "spoolerLog" in {
    val spoolerLog = new IDispatch.Stub {}
    val spoolerTask = new IDispatch.Stub {}
    val spoolerJob = new IDispatch.Stub {}
    val spooler = new IDispatch.Stub {}
    val namedIDispatches = TypedNamedIDispatches(List(
      "spooler_log" → spoolerLog,
      "spooler_task" → spoolerTask,
      "spooler_job" → spoolerJob,
      "spooler" → spooler))
    assert(namedIDispatches.toMap.size == 4)
  }

  "invalid name" in {
    intercept[IllegalArgumentException] { TypedNamedIDispatches(List("invalid" → new IDispatch.Stub {})) }
  }
}
