package js7.base.utils

import js7.base.test.OurTestSuite
import js7.base.time.ScalaTime.*

final class DelayConfTest extends OurTestSuite:

  "stream" in:
    assert:
      DelayConf(1.s).stream.take(3).toList == List(1.s, 1.s, 1.s)
    assert:
      DelayConf.default.stream.take(6).toList == List(1.s, 3.s, 6.s, 10.s, 10.s, 10.s)

  "lazyList" in:
    assert:
      DelayConf(1.s).lazyList.take(3).toList == List(1.s, 1.s, 1.s)
    assert:
      DelayConf.default.lazyList.take(6).toList == List(1.s, 3.s, 6.s, 10.s, 10.s, 10.s)
