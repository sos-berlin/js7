package com.sos.scheduler.engine.taskserver.module

import com.sos.scheduler.engine.minicom.idispatch.Invocable
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class NamedInvocablesTest extends FreeSpec {

  "spoolerLog" in {
    val spoolerLog = new Invocable {}
    val spoolerTask = new Invocable {}
    val spoolerJob = new Invocable {}
    val spooler = new Invocable {}
    val namedInvocables = NamedInvocables(List(
      "spooler_log" → spoolerLog,
      "spooler_task" → spoolerTask,
      "spooler_job" → spoolerJob,
      "spooler" → spooler))
    assert(namedInvocables.toMap.size == 4)
  }

  "invalid name" in {
    intercept[IllegalArgumentException] { NamedInvocables(List("invalid" → new Invocable {})) }
  }
}
