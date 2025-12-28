package js7.base.utils

import cats.effect.SyncIO
import js7.base.test.OurTestSuite
import js7.base.utils.Missing.*

final class MissingTest extends OurTestSuite:

  "getOrElse" in:
    val a: Int | Missing = 7
    assert(a.getOrElse(0) == 7)

    val missing: Int | Missing = Missing
    assert(missing.getOrElse(0) == 0)

  "toOption" in:
    val a: Int | Missing = 7
    assert(a.toOption == Some(7))

    val missing: Int | Missing = Missing
    assert(missing.toOption == None)

  "map" in:
    val a: Int | Missing = 7
    assert(a.map(_.toString) == "7")

    val missing: Int | Missing = Missing
    assert(missing.map(_.toString) == Missing)

  "foldMap" in:
    val a: Int | Missing = 7
    assert(a.foldMap(SyncIO(_)).unsafeRunSync() == 7)
    assert(a.foldMap(_ => SyncIO(Missing)).unsafeRunSync() == Missing)

    val missing: Int | Missing = Missing
    assert(missing.foldMap(SyncIO(_)).unsafeRunSync() == Missing)

  "foldMap requires Missing" in:
    // Should not compile because foldMap is considered to be used with Missing
    pendingUntilFixed:
      assertDoesNotCompile:
         """7.foldMap(SyncIO(_))"""

  "foreach" in:
    val a: Int | Missing = 7
    var result = -1
    a.foreach: o =>
      result = o
    assert(result == 7)

    val missing: Int | Missing = Missing
    missing.foreach: o =>
      fail()
