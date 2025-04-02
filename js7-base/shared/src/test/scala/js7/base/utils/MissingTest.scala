package js7.base.utils

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

  "foreach" in:
    val a: Int | Missing = 7
    var result = -1
    a.foreach: o =>
      result = o
    assert(result == 7)

    val missing: Int | Missing = Missing
    missing.foreach: o =>
      fail()
