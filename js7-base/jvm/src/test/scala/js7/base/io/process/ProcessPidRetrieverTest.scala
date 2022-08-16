package js7.base.io.process

import js7.base.io.process.ProcessPidRetriever.*
import js7.base.test.Test

/**
  * @author Joacim Zschimmer
  */
final class ProcessPidRetrieverTest extends Test
{
  "maybeOwnPid" in {
    assert(maybeOwnPid.isDefined == hasJava9)
  }
}
