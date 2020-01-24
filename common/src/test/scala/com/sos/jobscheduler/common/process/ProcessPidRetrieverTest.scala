package com.sos.jobscheduler.common.process

import com.sos.jobscheduler.common.process.ProcessPidRetriever._
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class ProcessPidRetrieverTest extends FreeSpec
{
  "maybeOwnPid" in {
    assert(maybeOwnPid.isDefined == hasJava9)
  }
}
