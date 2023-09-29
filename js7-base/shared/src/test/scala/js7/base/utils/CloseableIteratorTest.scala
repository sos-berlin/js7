package js7.base.utils

import js7.base.test.OurTestSuite
import js7.base.utils.CloseableIteratorTest.*

/**
  * @author Joacim Zschimmer
  */
final class CloseableIteratorTest extends OurTestSuite
{
  "strict" in {
    val a = new TestIterator(Iterator(1, 2, 3))
    assert(!a.closed)
    assert(a.strict == Vector(1, 2, 3))
    assert(a.closed)
  }

  ":+" in {
    val a = new TestIterator(Iterator(1, 2, 3))
    val b = a :+ 11
    assert(b.toVector == Vector(1, 2, 3, 11))
    assert(!a.closed)
    b.close()
    assert(a.closed)
  }

  "+:" in {
    val a = new TestIterator(Iterator(1, 2, 3))
    val b = -1 +: a
    assert(b.toVector == Vector(-1, 1, 2, 3))
    assert(!a.closed)
    b.close()
    assert(a.closed)
  }

  "++" in {
    val a = new TestIterator(Iterator(1, 2, 3))
    var lazyBEvaluated = false
    lazy val lazyB = new TestIterator(Iterator(11, 12))
    def b = {
      if lazyBEvaluated then fail("lazyB is evaluated twice")
      lazyBEvaluated = true
      lazyB
    }
    val c = a ++ b
    assert(c.next() == 1)
    assert(c.next() == 2)
    assert(c.next() == 3)
    assert(!lazyBEvaluated)
    assert(c.next() == 11)
    assert(lazyBEvaluated)
    assert(c.next() == 12)
    assert(!a.closed)
    assert(!lazyB.closed)
    c.close()
    assert(a.closed)
    assert(lazyB.closed)
  }

  "closeAtEnd" in {
    val a = new TestIterator(Iterator(1, 2))
    val b = a.closeAtEnd
    assert(b.next() == 1)
    assert(!a.closed)
    assert(b.next() == 2)
    assert(!a.closed)
    assert(!b.hasNext)  // closes automatically
    assert(a.closed)
  }

  "closeAtEnd on empty iterator closes immediately" in {
    val a = new TestIterator(Iterator.empty)
    assert(!a.closed)
    a.closeAtEnd  // Value discarded, but iterator is nevertheless closed immediately
    assert(a.closed)
  }

  "closeAtEnd with NoSuchElementException" in {
    val a = new TestIterator(Iterator(1, 2))
    val b = a.closeAtEnd
    assert(b.next() == 1)
    assert(!a.closed)
    assert(b.next() == 2)
    assert(!a.closed)
    intercept[NoSuchElementException] { b.next() }  // closes automatically
    assert(a.closed)
  }

  "closeAtEnd ++" in {
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

  "closeAtEnd is idempotent" in {
    val a = new TestIterator(Iterator(1, 2, 3)).closeAtEnd
    assert(a.closeAtEnd eq a)
  }

  "onClosed" in {
    var closed = false
    val a = new TestIterator(Iterator(1)) onClosed { closed = true }
    assert(!closed)
    a.next()
    assert(!a.hasNext)
    assert(!closed)
    a.close()
    assert(closed)
  }

  "fromCloseable" in {
    var closed = false
    val iterator = Iterator(1)
    val closeable = new AutoCloseable {
      def close() = closed = true
    }
    val closeableIterator = CloseableIterator.fromCloseable(closeable)(_ => iterator)
    assert(!closed)

    assert(closeableIterator.toList == 1 :: Nil)
    assert(!closed)

    closeableIterator.close()
    assert(closed)
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
