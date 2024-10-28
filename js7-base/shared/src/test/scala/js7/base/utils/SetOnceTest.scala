package js7.base.utils

import cats.effect.SyncIO
import js7.base.problem.{Problem, ProblemException}
import js7.base.test.OurTestSuite
import js7.base.utils.SetOnceTest.*

/**
 * @author Joacim Zschimmer
 */
final class SetOnceTest extends OurTestSuite:

  "SetOnce" in:
    val a = SetOnce[Int]
    assert(a.isEmpty)
    assert(!a.nonEmpty)
    assert(!a.isDefined)
    assert(intercept[ProblemException] { a.orThrow } .getMessage == "SetOnce[Int] promise has not been kept so far")
    assert(a.toOption == None)
    assert(a.getOrElse(-1) == -1)
    a := 0
    assert(!a.isEmpty)
    assert(a.nonEmpty)
    assert(a.isDefined)
    assert(a.orThrow == 0)
    assert(a.toOption == Some(0))
    assert(a.getOrElse(-1) == -0)
    assert(intercept[IllegalStateException] { a := 0 } .getMessage == "SetOnce[Int] has already been set")
    assert((for i <- a.toOption yield (i: Int) + 3) == Some(3))
    var r = 7
    for _ <- a do r = a.orThrow
    assert(r == 0)

  "getOrUpdate" in:
    val counter = Atomic(0)
    val a = SetOnce[A]
    assert(a.getOrUpdate(A(counter.incrementAndGet())) == A(1))
    assert(a.getOrUpdate(A(counter.incrementAndGet())) == A(1))
    assert(a.getOrUpdate(sys.error("lazy")) == A(1))
    assert(counter.get() == 1)

  "checked" in:
    val a = SetOnce[Int]
    assert(a.checked == Left(Problem.pure("SetOnce[Int] promise has not been kept so far")))
    a := 7
    assert(a.checked == Right(7))

  "whenDefined" in :
    val a = Lazy(7)
    var called: Int = -1

    def f(i: Int): SyncIO[Unit] =
      SyncIO:
        called = i

    a.whenDefined(f).unsafeRunSync()
    assert(a() == 7)
    assert(called == -1)

    a.whenDefined(f).unsafeRunSync()
    assert(called == 7)

  "toOption" in:
    val a = SetOnce[Int]
    assert(a.toOption == None)
    a := 7
    assert(a.toOption == Some(7))

  "fold" in:
    val a = SetOnce[Int]
    assert(a.fold(0)(_ * 3) == 0)
    a := 7
    assert(a.fold(0)(_ * 3) == 21)

  "toString" in:
    val a = SetOnce[Int]
    assert(a.toString == "SetOnce[Int](not yet set)")
    a := 7
    assert(a.toString == "7")


object SetOnceTest:
  private case class A(number: Int)
