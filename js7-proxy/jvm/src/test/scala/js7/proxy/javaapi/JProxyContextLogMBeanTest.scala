package js7.proxy.javaapi

import cats.effect.IO
import java.lang.management.ManagementFactory
import javax.management.ObjectName
import js7.base.io.file.FileUtils.syntax.*
import js7.base.io.file.FileUtils.withTemporaryDirectory
import js7.base.test.OurAsyncTestSuite
import js7.base.utils.Tests.isIntelliJIdea

final class JProxyContextLogMBeanTest extends OurAsyncTestSuite:

  "test" in :
    if !isIntelliJIdea then
      pending
    else
      JProxyContext.resource().use: jProxyContext =>
        IO.blocking:
          withTemporaryDirectory("JProxyContextLogMBeanTest-"): logDirectory =>
            logDirectory / "test.log" := "+" * 999
            jProxyContext.registerLogDirectoryMBean(logDirectory)
            val beanName = new ObjectName("js7:type=LogDirectory")
            assert:
              ManagementFactory.getPlatformMBeanServer.getAttribute(beanName, "Size") == 999.0
