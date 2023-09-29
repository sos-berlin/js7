package js7.common.scalautil

import js7.base.test.OurTestSuite
import js7.base.utils.HasCloser
import scala.util.Success

/**
 * @author Joacim Zschimmer
 */
final class ClosedFutureTest extends OurTestSuite:

  "closed future succeeds" in:
    object o extends HasCloser with ClosedFuture
    assert(!o.closed.isCompleted)
    o.close()
    assert(o.closed.isCompleted)
    assert(o.closed.value == Some(Success(())))

  "closed future succeeds event if a close call throws an exception" in:
    object o extends HasCloser with ClosedFuture:
      onClose { throw new RuntimeException }
    assert(!o.closed.isCompleted)
    intercept[RuntimeException] { o.close() }
    assert(o.closed.isCompleted)
    assert(o.closed.value == Some(Success(())))
