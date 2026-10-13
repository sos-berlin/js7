package js7.base.log.log4j

import js7.base.test.OurTestSuite

final class Log4jTest extends OurTestSuite:

  "heapSizeToRingBufferSize" in :
    assert(Log4j.heapSizeToRingBufferSize(-1) == 1)
    assert(Log4j.heapSizeToRingBufferSize(0) == 1)
    assert(Log4j.heapSizeToRingBufferSize(1) == 1)
    assert(Log4j.heapSizeToRingBufferSize(50 * 1024 * 1024) == 2048)
    assert(Log4j.heapSizeToRingBufferSize(100 * 1024 * 1024) == 4096)
    assert(Log4j.heapSizeToRingBufferSize(Long.MaxValue) == 4096)

