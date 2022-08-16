package js7.base.io.process

import js7.base.io.process.ProcessPidRetriever.*
import js7.base.test.OurTestSuite

/**
  * @author Joacim Zschimmer
  */
final class ProcessPidRetrieverTest extends OurTestSuite
{
  "maybeOwnPid" in {
    assert(maybeOwnPid.isDefined == hasJava9)
  }
}
