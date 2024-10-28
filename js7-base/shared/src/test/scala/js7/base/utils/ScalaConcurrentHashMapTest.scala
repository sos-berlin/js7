package js7.base.utils

import js7.base.test.OurTestSuite

/**
 * @author Joacim Zschimmer
 */
final class ScalaConcurrentHashMapTest extends OurTestSuite:
  private val m = new ScalaConcurrentHashMap[Int, String]

  "insert" in:
    m.insert(1 -> "eins")
    intercept[DuplicateKeyException] { m.insert(1 -> "eins") }

  "+=" in:
    m.update(1, "eins")
    m.update(2, "zwei")
    m.update(1, "EINS")
    m.update(3, "drei")
    assert(m == Map(1 -> "EINS", 2 -> "zwei", 3 -> "drei"))

  "-=" in:
    m -= 1
    assert(m == Map(2 -> "zwei", 3 -> "drei"))
    m -= 1
    assert(m == Map(2 -> "zwei", 3 -> "drei"))

  "iterator" in:
    assert(m.iterator.toMap == Map(2 -> "zwei", 3 -> "drei"))

  "get" in:
    assert(m.get(1) == None)
    assert(m.get(2) == Some("zwei"))

  "getOrElse" in:
    assert(m.getOrElse(1, "DEFAULT") == "DEFAULT")
    assert(m.getOrElse(2, sys.error("")) == "zwei")

  "apply" in:
    intercept[NoSuchElementException] { m(1) }
    assert(m(2) == "zwei")
    assert(m(3) == "drei")

  "contains" in:
    assert(!m.contains(1))
    assert(m.contains(2))
    assert(m.contains(3))

  "isEmpty" in:
    assert(!m.isEmpty)
    assert(new ScalaConcurrentHashMap[Int, Int].isEmpty)

  "keys" in:
    assert(m.keys.toSet == Set(2, 3))

  "keySet" in:
    assert(m.keySet == Set(2, 3))

  "keysIterator" in:
    assert(m.keysIterator.toSet == Set(2, 3))

  "values" in:
    assert(m.values.toSet == Set("zwei", "drei"))

  "valuesIterator" in:
    assert(m.valuesIterator.toSet == Set("zwei", "drei"))
