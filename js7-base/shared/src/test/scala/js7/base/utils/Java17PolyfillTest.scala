package js7.base.utils

import js7.base.system.Java17Polyfill.*
import js7.base.test.OurTestSuite
import scala.annotation.nowarn

final class Java17PolyfillTest extends OurTestSuite:

  java17Polyfill() // require import

  "javaVersion" in:
    assert(Thread.currentThread.threadId ==
      (Thread.currentThread().getId(): @nowarn("msg=deprecated")))

  "CharSequence" - {
    "getChars" in:
      val string = "abcd"
      val array = Array.fill(4)('•')

      (string: CharSequence).getChars(1, 3, array, 1)
      assert(array.mkString == "•bc•")

      assert:
        (string: CharSequence).getChars(1, 3, array, 1) == string.getChars(1, 3, array, 1)
  }
