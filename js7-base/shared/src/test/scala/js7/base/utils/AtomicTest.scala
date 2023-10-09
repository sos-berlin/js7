package js7.base.utils

import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger, AtomicLong, AtomicReference}
import js7.base.test.OurTestSuite
import js7.base.utils.Atomic.syntax.*

final class AtomicTest extends OurTestSuite:

  "AtomicBoolean" in:
    val a: AtomicBoolean = Atomic(false)
    assert(!a.getAndSet(true))
    assert(a.compareAndSet(true, false))
    assert(!a.get())
    a := true
    assert(a.get())

  "AtomicInteger" in:
    val a: AtomicInteger = Atomic(1)
    assert(a.getAndSet(2) == 1)
    assert(a.compareAndSet(2, 3))
    assert(a.get() == 3)
    a := 4
    assert(a.get() == 4)
    assert((a += 1) == 5)
    assert((a -= 2) == 3)

  "AtomicLong" in:
    val a: AtomicLong = Atomic(1L)
    assert(a.getAndSet(2L) == 1L)
    assert(a.compareAndSet(2L, 3L))
    assert(a.get() == 3L)
    a := 4L
    assert(a.get() == 4L)
    assert((a += 1L) == 5L)
    assert((a -= 2L) == 3L)

  "AtomicReference" in:
    val a: AtomicReference[String] = Atomic("1")
    assert(a.getAndSet("2") == "1")
    assert(a.compareAndSet("2", "3"))
    assert(a.get() == "3")
    a := "4"
    assert(a.get() == "4")
