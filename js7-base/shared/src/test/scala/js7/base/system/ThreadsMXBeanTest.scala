package js7.base.system

import js7.base.test.OurTestSuite

final class ThreadsMXBeanTest extends OurTestSuite:

  "test" in:
    assert(ThreadsMXBean.Bean.getOtherThreads > 0)
