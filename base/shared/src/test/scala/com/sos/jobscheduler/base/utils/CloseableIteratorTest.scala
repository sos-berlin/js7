package com.sos.jobscheduler.base.utils

import com.sos.jobscheduler.base.utils.CloseableIteratorTest._
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class CloseableIteratorTest extends FreeSpec
{
  "strict" in {
    val a = new TestIterator(Iterator(1, 2, 3))
    assert(a.strict == Vector(1, 2, 3))
    assert(a.closed)
  }

  ":+" in {
    val a = new TestIterator(Iterator(1, 2, 3))
    assert((a :+ 11).toVector == Vector(1, 2, 3, 11))
  }

  "++" in {
    val a = new TestIterator(Iterator(1, 2, 3))
    var lazyBTouched = false
    lazy val b = {
      lazyBTouched = true
      new TestIterator(Iterator(11, 12))
    }
    val c = a ++ b
    assert(c.next() == 1)
    assert(c.next() == 2)
    assert(c.next() == 3)
    assert(!lazyBTouched)
    assert(c.next() == 11)
    assert(lazyBTouched)
    assert(c.next() == 12)
    assert(!a.closed)
    assert(!b.closed)
    c.close()
    assert(a.closed)
    assert(b.closed)
  }

  "closeAtEnd" in {
    val a = new TestIterator(Iterator(1, 2, 3))
    val b = new TestIterator(Iterator(11, 12))
    val c = a.closeAtEnd ++ b.closeAtEnd
    assert(c.next() == 1)
    assert(!a.closed && !b.closed)
    assert(c.next() == 2)
    assert(!a.closed && !b.closed)
    assert(c.next() == 3)
    assert(!a.closed && !b.closed)
    assert(c.next() == 11)
    assert(a.closed && !b.closed)
    assert(c.next() == 12)
    assert(a.closed && !b.closed)
    assert(!c.hasNext)
    assert(a.closed && b.closed)
  }
}

object CloseableIteratorTest {
  private final class TestIterator(iterator: Iterator[Int]) extends CloseableIterator[Int] {
    var closed = false

    def hasNext = {
      assert(!closed)
      iterator.hasNext
    }

    def next() = {
      assert(!closed)
      iterator.next()
    }

    def close(): Unit = {
      closed = true
    }
  }
}
