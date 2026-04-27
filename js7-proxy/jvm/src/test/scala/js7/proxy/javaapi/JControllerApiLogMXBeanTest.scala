package js7.proxy.javaapi

import cats.effect.IO
import java.lang.management.ManagementFactory
import javax.management.ObjectName
import js7.base.auth.Admission
import js7.base.io.file.FileUtils.syntax.RichPath
import js7.base.io.file.FileUtils.withTemporaryDirectory
import js7.base.test.OurAsyncTestSuite
import js7.base.utils.CatsUtils.Nel
import js7.base.utils.Tests.isIntelliJIdea
import js7.base.web.Uri

final class JControllerApiLogMXBeanTest extends OurAsyncTestSuite:

  "test" in:
    if !isIntelliJIdea then
      pending
    else
      JProxyContext.resource().use: jProxy =>
        jProxy.controllerApiResource(Nel.of(Admission(Uri("http://127.0.0.1:0")))).use: jControllerApi =>
          IO:
            withTemporaryDirectory("JControllerApiLogMXBeanTest-"): logDirectory =>
              jControllerApi.registerLogDirectoryMxBean(logDirectory = logDirectory)
              logDirectory / "test.log" := "+" * 999
              val beanName = new ObjectName("js7:type=LogDirectory")
              assert:
                ManagementFactory.getPlatformMBeanServer.getAttribute(beanName, "Size") == 999.0
