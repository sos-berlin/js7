package com.sos.jobscheduler.common.process

import com.sos.jobscheduler.common.process.ProcessPidRetriever._
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class ProcessPidRetrieverTest extends AnyFreeSpec
{
  "maybeOwnPid" in {
    assert(maybeOwnPid.isDefined == hasJava9)
  }
}
