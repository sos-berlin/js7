package com.sos.scheduler.engine.jobapi.test

import java.lang.Thread.sleep

/**
 * For user's tests.
 *
 * @author Joacim Zschimmer
 */
final class SleepJob extends sos.spooler.Job_impl {

  private var n = 10

  override def spooler_process() =
    n > 0 && {
      n -= 1
      sleep(1000)
      true
    }
}
