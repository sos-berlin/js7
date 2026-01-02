package js7.base.utils

import cats.effect.IO
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger, AtomicLong, AtomicReference}
import js7.base.Problems.ConcurrentAccessProblem
import js7.base.catsutils.CatsEffectExtensions.{left, right}
import js7.base.problem.Problem
import js7.base.test.OurAsyncTestSuite
import js7.base.utils.Atomic.extensions.*

final class AtomicTest extends OurAsyncTestSuite:

  "AtomicBoolean" - {
    "Standards" in :
      val a: AtomicBoolean = Atomic(false)
      assert(!a.getAndSet(true))
      assert(a.compareAndSet(true, false))
      assert(!a.get())
      a := true
      assert(a.get())

    "whenInUse" in :
      val active = Atomic(false)
      active.whenInUse:
        IO.left(Problem("IN USE 1"))
      .otherwiseUse:
        IO.defer:
          assert(active.get())
          active.whenInUse:
            IO.left(Problem("IN USE 2"))
          .otherwiseUse:
            IO.right(())
      .map: checked =>
        assert(checked == Left(Problem("IN USE 2")) && !active.get())

    "nonConcurrent with Checked" in :
      val active = Atomic(false)
      active.nonConcurrent("FIRST"):
        IO.defer:
          assert(active.get())
          active.nonConcurrent("SECOND"):
            IO.right(())
      .map: checked =>
        assert(checked == Left(ConcurrentAccessProblem("SECOND")) && !active.get())

    "nonConcurrent with Monoid" in :
      val active = Atomic(false)
      var innerReached = false
      active.nonConcurrent:
        IO.defer:
          innerReached = true
          assert(active.get())
          active.nonConcurrent:
            IO.pure(7)
      .map: result =>
        assert(innerReached && result == 0 && !active.get())
  }

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
