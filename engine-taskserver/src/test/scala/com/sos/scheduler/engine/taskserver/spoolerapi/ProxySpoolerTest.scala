package com.sos.scheduler.engine.taskserver.spoolerapi

import com.sos.scheduler.engine.minicom.idispatch.IDispatch.implicits._
import com.sos.scheduler.engine.minicom.remoting.calls.ProxyId
import com.sos.scheduler.engine.minicom.remoting.proxy.ProxyRemoting
import com.sos.scheduler.engine.taskserver.data.TaskServerArguments
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

  private lazy val spooler: ProxySpooler = new ProxySpooler.Factory {
      val taskServerArguments = TaskServerArguments.forTest(directory = testDirectory)
    } .apply(mock[ProxyRemoting], ProxyId(Random.nextLong()), name = "TEST", properties = Nil)

  "directory returns TaskServerArguments.directory" in {
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
