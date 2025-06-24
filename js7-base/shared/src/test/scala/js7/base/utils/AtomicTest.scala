package js7.base.utils

import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger, AtomicLong, AtomicReference}
import js7.base.test.OurTestSuite
import js7.base.utils.Atomic.extensions.*

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
    a += 1
    assert(a.get() == 5)
    a -= 2
    assert(a.get() == 3)

  "AtomicLong" in:
    val a: AtomicLong = Atomic(1L)
    assert(a.getAndSet(2L) == 1L)
    assert(a.compareAndSet(2L, 3L))
    assert(a.get() == 3L)
    a := 4L
    assert(a.get() == 4L)
    a += 1L
    assert(a.get() == 5L)
    a -= 2L
    assert(a.get() == 3L)

  "AtomicReference" in:
    val a: AtomicReference[String] = Atomic("1")
    assert(a.getAndSet("2") == "1")
    assert(a.compareAndSet("2", "3"))
    assert(a.get() == "3")
    a := "4"
    assert(a.get() == "4")
