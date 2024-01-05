package js7.common.utils

import js7.base.test.OurTestSuite

final class SimpleIteratorTest extends OurTestSuite:
  "test" in:
    val it = new SimpleIterator[Int]:
      private val elems = List(1, 2, 3).iterator
      protected def computeNext() = if elems.hasNext then elems.next() else endOfData
    assert(it.peek == 1)
    assert(it.hasNext)
    assert(it.next() == 1)
    assert(it.hasNext)
    assert(it.next() == 2)
    assert(it.peek == 3)
    assert(it.next() == 3)
    assert(!it.hasNext)
    intercept[NoSuchElementException](it.peek)
    intercept[NoSuchElementException](it.next())
    intercept[NoSuchElementException](it.next())
    assert(!it.hasNext)
