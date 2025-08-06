package js7.data.event

import cats.effect.IO
import java.lang.management.ManagementFactory
import js7.base.system.MBeanUtils.{registerMBean, toMBeanName}
import js7.base.test.OurAsyncTestSuite

final class EventCounterTest extends OurAsyncTestSuite:

  "EventCounterMXBean" in :
    val objectName = toMBeanName("EventCounterTest")
    registerMBean(objectName):
      IO:
        EventCounter(Map("OneEvent" -> 1, "OtherEvent" -> 2))
    .use: bean =>
      IO:
        val beanServer = ManagementFactory.getPlatformMBeanServer
        assert:
          beanServer.getAttribute(objectName, "EventTotal") == 3
