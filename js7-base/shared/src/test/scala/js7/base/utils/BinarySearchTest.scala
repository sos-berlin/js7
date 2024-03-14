package js7.base.utils

import js7.base.test.OurTestSuite
import js7.base.utils.BinarySearch.{binarySearch, binarySearchNew}

final class BinarySearchTest extends OurTestSuite:

  "binarySearch" in:
    locally:
      assert(binarySearch(Vector.empty)(7) == (0, false))

      assert(binarySearch(Vector(2))(1) == (0, false))
      assert(binarySearch(Vector(2))(2) == (0, true))
      assert(binarySearch(Vector(2))(3) == (1, false))

    locally:
      val seq = Vector(20, 30, 40)
      assert(binarySearch(seq)(15) == (0, false))
      assert(binarySearch(seq)(20) == (0, true))
      assert(binarySearch(seq)(25) == (1, false))
      assert(binarySearch(seq)(30) == (1, true))
      assert(binarySearch(seq)(35) == (2, false))
      assert(binarySearch(seq)(40) == (2, true))
      assert(binarySearch(seq)(56) == (3, false))

    locally:
      val n = 1_000_000
      val seq = Vector.from(0 until n).map(_.toString)

      var comparisons = 0
      val compare: String => Int => Int =
        searched =>
          Logger.trace("┈" * 30)
          comparisons = 0
          index =>
            comparisons += 1
            val r = searched.toInt.compare(seq(index).toInt)
            val cmp = if r < 0 then "<" else if r > 0 then ">" else "="
            Logger.trace(s"$searched $cmp ${seq(index)} ")
            r

      assert(binarySearchNew(0, 0)(compare("-1")) == (0, false) && comparisons == 0)
      assert(binarySearchNew(0, 1)(compare("-1")) == (0, false) && comparisons == 1)

      assert(binarySearchNew(0, 7)(compare("-1")) == (0, false) && comparisons == 3)
      assert(binarySearchNew(0, 7)(compare("0")) == (0, true) && comparisons == 3)
      assert(binarySearchNew(0, 7)(compare("1")) == (1, true) && comparisons == 2)
      assert(binarySearchNew(0, 7)(compare("2")) == (2, true) && comparisons == 3)
      assert(binarySearchNew(0, 7)(compare("3")) == (3, true) && comparisons == 1)
      assert(binarySearchNew(0, 7)(compare("4")) == (4, true) && comparisons == 3)
      assert(binarySearchNew(0, 7)(compare("5")) == (5, true) && comparisons == 2)
      assert(binarySearchNew(0, 7)(compare("6")) == (6, true) && comparisons == 3)
      assert(binarySearchNew(0, 7)(compare("7")) == (7, false) && comparisons == 3)

      assert(binarySearchNew(0, n)(compare("-1")) == (0, false) && comparisons == 20)
      assert(binarySearchNew(0, n)(compare("0")) == (0, true) && comparisons == 20)
      assert(binarySearchNew(0, n)(compare("1")) == (1, true) && comparisons == 19)
      assert(binarySearchNew(0, n)(compare("2")) == (2, true) && comparisons == 20)
      assert(binarySearchNew(0, n)(compare("3")) == (3, true) && comparisons == 18)
      assert(binarySearchNew(0, n)(compare("250000")) == (250000, true) && comparisons == 2)
      assert(binarySearchNew(0, n)(compare("499999")) == (499999, true) && comparisons == 19)
      assert(binarySearchNew(0, n)(compare("500000")) == (500000, true) && comparisons == 1)
      assert(binarySearchNew(0, n)(compare("1000000")) == (1000000, false) && comparisons == 19)
