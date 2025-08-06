package js7.base.system

import cats.effect.IO
import java.lang.management.ManagementFactory
import javax.management.ObjectName
import js7.base.system.MBeanUtils.registerMBean
import js7.base.system.MBeanUtilsTest.*
import js7.base.test.OurAsyncTestSuite

final class MBeanUtilsTest extends OurAsyncTestSuite:

  "registerMBean" in:
    val objectName = new ObjectName("js7:type=MBeanUtilsTest")
    locally:
      for
        _ <- registerMBean(objectName)(IO(Bean(111)))
        _ <- registerMBean(objectName)(IO(Bean(222)))
        _ <- registerMBean(objectName)(IO(Bean(333)))
      yield ()
    .surround:
      IO:
        val beanServer = ManagementFactory.getPlatformMBeanServer
        assert:
          beanServer.getAttribute(objectName, "Value") == 111

object MBeanUtilsTest:
  private sealed trait MXBean:
    def getValue: Int

  private final class Bean(value: Int) extends MXBean:
    def getValue = value
