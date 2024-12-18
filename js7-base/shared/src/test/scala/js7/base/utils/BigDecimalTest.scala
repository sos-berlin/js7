package js7.base.utils

import js7.base.test.OurTestSuite

final class BigDecimalTest extends OurTestSuite:

  "toLong does not convert numerically (but bitwise)" in:
    assert((BigDecimal(Long.MaxValue) + 1).toLong == Long.MinValue)
