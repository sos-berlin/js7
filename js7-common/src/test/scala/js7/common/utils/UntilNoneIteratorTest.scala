package js7.common.utils

import js7.base.test.Test

/**
  * @author Joacim Zschimmer
  */
final class UntilNoneIteratorTest extends Test {

  "test" in {
    assert(!untilNoneIterator[Int](None).hasNext)
    assert(untilNoneIterator[Int](Some(3)).hasNext)
    assert(untilNoneIterator[Int](Some(3)).next() == 3)
  }

  "Exception is delayed until next()" in {
    def read(): Option[Int] = throw new IndexOutOfBoundsException()
    assert(untilNoneIterator(read()).hasNext)
    intercept[IndexOutOfBoundsException] {
      untilNoneIterator(read()).next()
    }
  }
}
