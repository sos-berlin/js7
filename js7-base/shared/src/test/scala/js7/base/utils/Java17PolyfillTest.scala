package js7.base.utils

import js7.base.system.Java17Polyfill.*
import js7.base.test.OurTestSuite
import scala.annotation.nowarn

final class Java17PolyfillTest extends OurTestSuite:

  java17Polyfill() // require import

  "javaVersion" in:
    assert(Thread.currentThread.threadId ==
      (Thread.currentThread().getId(): @nowarn("msg=deprecated")))

  "indexOf" in:
    val string = "axcdx"
    assert(string.indexOf('x', 0, 3) == 1)
    assert(string.indexOf('x', 0, 2) == 1)
    assert(string.indexOf('x', 0, 1) == -1)
    assert(string.indexOf('a', 0, 0) == -1)
    assert(string.indexOf('x', 1, 5) == 1)
    assert(string.indexOf('x', 2, 5) == 4)
    intercept[StringIndexOutOfBoundsException]:
      string.indexOf('x', 0, 6)
    intercept[StringIndexOutOfBoundsException]:
      string.indexOf('x', 6, 5)
    intercept[StringIndexOutOfBoundsException]:
      string.indexOf('x', 1, 0)

  "CharSequence" - {
    "getChars" in:
      val string = "abcd"
      val array = Array.fill(4)('•')

      (string: CharSequence).getChars(1, 3, array, 1)
      assert(array.mkString == "•bc•")

      assert:
        (string: CharSequence).getChars(1, 3, array, 1) == string.getChars(1, 3, array, 1)
  }
