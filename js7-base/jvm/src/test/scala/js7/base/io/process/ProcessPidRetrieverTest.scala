package js7.base.io.process

import js7.base.io.process.ProcessPidRetriever.*
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
