package js7.base.utils

import cats.effect.SyncIO
import js7.base.test.OurTestSuite

/**
  * @author Joacim Zschimmer
  */
final class LazyTest extends OurTestSuite:

  "Lazy" in:
    var counter = 0
    val a = Lazy({ counter += 1; 7 })
    assert(!a.isDefined)
    for _ <- a do fail()
    //assert(a.map(_ * 3 ) == None)
    assert(a.toOption == None)
    assert(a.value == 7)
    assert(counter == 1)
    assert(a.toOption == Some(7))
    assert(a.isDefined)
    var foreachCalled = 0
    for x <- a do foreachCalled += x
    assert(foreachCalled == 7)
    //assert(a.map(_ * 3 ) == Some(21))
    assert(a.value == 7)
    assert(a.value == 7)
    assert(a.recursionCheckedValue == Some(7))
    assert(a() == 7)
    assert(a() == 7)
    assert(counter == 1)

  "Recursion" in:
    lazy val a: Lazy[Nothing] = Lazy(a.value)
    intercept[a.RecursiveLazyValueException]:
      a.value
    assert(a.recursionCheckedValue == None)

    val b = Lazy(a.value)
    intercept[a.RecursiveLazyValueException]:
      b.value
    assert(b.recursionCheckedValue == None)

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

  "fold" in :
    val a = Lazy(7)
    assert(a.fold(0)(_ * 3) == 0)
    a()
    assert(a.fold(0)(_ * 3) == 21)

  "toOption" in :
    val a = Lazy(7)
    assert(a.toOption == None)
    assert(a() == 7)
    assert(a.toOption == Some(7))
