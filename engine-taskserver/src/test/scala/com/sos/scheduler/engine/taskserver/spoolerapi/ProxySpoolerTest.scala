package com.sos.scheduler.engine.taskserver.spoolerapi

import com.google.inject.Guice
import com.sos.scheduler.engine.common.guice.ScalaAbstractModule
import com.sos.scheduler.engine.minicom.idispatch.IDispatch.implicits._
import com.sos.scheduler.engine.minicom.idispatch.InvocableIDispatch
import com.sos.scheduler.engine.minicom.remoting.calls.ProxyId
import com.sos.scheduler.engine.minicom.remoting.proxy.ClientRemoting
import com.sos.scheduler.engine.taskserver.task.TaskStartArguments
import java.io.File
import java.nio.file.Paths
import org.junit.runner.RunWith
import org.scalatest.Matchers._
import org.scalatest.junit.JUnitRunner
import org.scalatest.mock.MockitoSugar.mock
import org.scalatest.{BeforeAndAfterAll, FreeSpec}
import scala.util.Random

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class ProxySpoolerTest extends FreeSpec with BeforeAndAfterAll {

  private val testDirectory = Paths.get("TEST")
  private lazy val injector = Guice.createInjector(new ScalaAbstractModule {
    def configure() = bindInstance[TaskStartArguments](TaskStartArguments.forTest(directory = testDirectory))
  })

  private lazy val spooler = InvocableIDispatch(
    ProxySpooler(injector, mock[ClientRemoting], ProxyId(Random.nextLong()), name = "TEST", properties = Nil))

  "directory returns TaskStartArguments.directory" in {
    assert(spooler.invokeGet("directory") == s"$testDirectory${File.separator}")
  }

  for (unsupportedGetter ← List("include_path", "ini_path", "log_dir")) {
    s"$unsupportedGetter is not supported" in {
      intercept[UnsupportedOperationException] {spooler.invokeGet(unsupportedGetter)}
        .getMessage shouldEqual s"Universal Agent does not support method 'sos.spooler.Spooler.$unsupportedGetter'"
    }
  }

  "create_xslt_stylesheet is not supported" in {
    intercept[UnsupportedOperationException] { spooler.call("create_xslt_stylesheet") }
    intercept[UnsupportedOperationException] { spooler.call("create_xslt_stylesheet", List("file")) }
  }
}
