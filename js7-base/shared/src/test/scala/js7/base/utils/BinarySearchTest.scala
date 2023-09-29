package js7.base.utils

import js7.base.test.OurTestSuite
import js7.base.utils.BinarySearch.binarySearch

final class BinarySearchTest extends OurTestSuite:
  "binarySearch" in:
    assert(binarySearch(Vector.empty, 7) == (0, false))

    assert(binarySearch(Vector(30), 20) == (0, false))
    assert(binarySearch(Vector(30), 30) == (0, true))
    assert(binarySearch(Vector(30), 40) == (1, false))

    assert(binarySearch(Vector(20, 30, 40), 15) == (0, false))
    assert(binarySearch(Vector(20, 30, 40), 20) == (0, true))
    assert(binarySearch(Vector(20, 30, 40), 25) == (1, false))
    assert(binarySearch(Vector(20, 30, 40), 30) == (1, true))
    assert(binarySearch(Vector(20, 30, 40), 35) == (2, false))
    assert(binarySearch(Vector(20, 30, 40), 40) == (2, true))
    assert(binarySearch(Vector(20, 30, 40), 56) == (3, false))
