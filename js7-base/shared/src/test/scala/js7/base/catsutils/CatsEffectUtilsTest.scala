package js7.base.catsutils

import cats.effect.IO
import cats.syntax.foldable.*
import cats.syntax.monoid.*
import js7.base.test.OurAsyncTestSuite

final class CatsEffectUtilsTest extends OurAsyncTestSuite:

  "Monoid[IO[Unit]]" in :
    var _a = 0
    val a = IO(_a += 1)
    var _b = 0
    val b = IO(_b += 1)
    (a |+| b).map: _ =>
      assert(_a == 1 && _b == 1)
    .productR:
      var x = 0
      val n = 10
      (1 to n).map(i => IO(x += i)).toVector
        .combineAll(using CatsEffectUtils.UnitIOMonoid).map: _ =>
          assert(x == n * (n + 1) / 2)

  "Monoid[IO[x]]" in :
    val n = 10
    (1 to n).map(i => IO(i)).toVector
      .combineAll(using CatsEffectUtils.ioMonoid).map: sum =>
        assert(sum == n * (n + 1) / 2)
