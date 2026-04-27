package js7.journal

import cats.effect.IO
import java.lang.management.ManagementFactory
import js7.base.system.MBeanUtils.{registerMBean, toMBeanName}
import js7.base.test.OurAsyncTestSuite

final class FileJournalMXBeanTest extends OurAsyncTestSuite:

  "FileSize" in :
    val beanName = toMBeanName("FileJournalMXBeanTest-Journal")
    registerMBean(beanName)(IO(FileJournalMXBean.Bean(None))).use: bean =>
      IO:
        bean.fileSize = 123
        assert:
          ManagementFactory.getPlatformMBeanServer.getAttribute(beanName, "FileSize") == 123.0
