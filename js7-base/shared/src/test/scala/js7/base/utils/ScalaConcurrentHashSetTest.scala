package js7.base.utils

import js7.base.test.OurTestSuite

/**
 * @author Joacim Zschimmer
 */
final class ScalaConcurrentHashSetTest extends OurTestSuite:
  private val m = new ScalaConcurrentHashSet[Int]

  "insert" in:
    m.insert(1)
    intercept[DuplicateKeyException] { m.insert(1) }

  "+=" in:
    m += 1
    m += 2
    m += 1
    m += 3
    assert(m == Set(1, 2, 3))

  "-=" in:
    m -= 1
    assert(m == Set(2, 3))
    m -= 1
    assert(m == Set(2, 3))

  "iterator" in:
    assert(m.iterator.toSet == Set(2, 3))

  "apply" in:
    assert(!m(1))
    assert(m(2))
    assert(m(3))

  "contains" in:
    assert(!m.contains(1))
    assert(m.contains(2))
    assert(m.contains(3))

  "isEmpty" in:
    assert(!m.isEmpty)
    assert(new ScalaConcurrentHashMap[Int, Int].isEmpty)
