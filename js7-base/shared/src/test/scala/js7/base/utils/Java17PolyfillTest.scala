package js7.base.utils

import js7.base.system.Java17Polyfill.*
import js7.base.test.OurTestSuite
import scala.annotation.nowarn

final class Java17PolyfillTest extends OurTestSuite:

  "javaVersion" in:
    assert(Thread.currentThread.threadId ==
      (Thread.currentThread().getId(): @nowarn("msg=deprecated")))

  java17Polyfill()
