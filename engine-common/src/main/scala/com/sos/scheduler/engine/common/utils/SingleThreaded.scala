package com.sos.scheduler.engine.common.utils

import java.lang.Thread.currentThread
import org.scalactic.Requirements._

/**
 * @author Joacim Zschimmer
 */
trait SingleThreaded {
  protected final val myThread = currentThread

  protected def requireMyThread(): Unit = {
    requireState(currentThread eq myThread,
      s"SingleThreaded object has been created in thread '$myThread', but tried to be used in '$currentThread'. Object: $toString")
  }
}
