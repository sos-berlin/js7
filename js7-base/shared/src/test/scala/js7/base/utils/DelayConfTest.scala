package js7.base.utils

import js7.base.test.OurTestSuite
import js7.base.time.ScalaTime.*

final class DelayConfTest extends OurTestSuite:

  "iterator" in:
    assert:
      DelayConf.default.iterator().take(6).toSeq == Seq(1.s, 3.s, 6.s, 10.s, 10.s, 10.s)
