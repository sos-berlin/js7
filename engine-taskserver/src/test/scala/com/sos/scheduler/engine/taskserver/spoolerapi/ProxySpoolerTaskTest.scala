package com.sos.scheduler.engine.taskserver.spoolerapi

import com.google.inject.Guice
import com.sos.scheduler.engine.common.guice.ScalaAbstractModule
import com.sos.scheduler.engine.common.scalautil.Closers.implicits._
import com.sos.scheduler.engine.common.scalautil.FileUtils.implicits.RichPath
import com.sos.scheduler.engine.common.scalautil.HasCloser
import com.sos.scheduler.engine.common.system.FileUtils._
import com.sos.scheduler.engine.minicom.idispatch.IDispatch.implicits._
import com.sos.scheduler.engine.minicom.idispatch._
import com.sos.scheduler.engine.minicom.remoting.calls.ProxyId
import com.sos.scheduler.engine.minicom.remoting.proxy.ClientRemoting
import com.sos.scheduler.engine.taskserver.data.TaskServerConfiguration._
import com.sos.scheduler.engine.taskserver.data.TaskStartArguments
import com.sos.scheduler.engine.taskserver.task.process.RichProcess
import com.sos.scheduler.engine.taskserver.task.process.StdoutStderr.{Stderr, Stdout}
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
final class ProxySpoolerTaskTest extends FreeSpec with BeforeAndAfterAll with HasCloser {

  private lazy val stdFileMap = {
    val r = RichProcess.createStdFiles(temporaryDirectory, id = "ProxySpoolerTaskTest")
    closer.onClose { RichProcess.tryDeleteFiles(r.values) }
    r
  }

  private lazy val injector = Guice.createInjector(new ScalaAbstractModule {
    def configure() = bindInstance[TaskStartArguments](TaskStartArguments.forTest(stdFileMap = stdFileMap))
  })

  private lazy val spoolerTask = InvocableIDispatch(
    ProxySpoolerTask(injector, mock[ClientRemoting], ProxyId(Random.nextLong()), name = "TEST", properties = Nil))

  override def afterAll() = closer.close()

  "stdout_path" in {
    assert(spoolerTask.invokeGet("stdout_path") == stdFileMap(Stdout).toString)
  }

  "stderr_path" in {
    assert(spoolerTask.invokeGet("stderr_path") == stdFileMap(Stderr).toString)
  }

  "stdout_text" in {
    val string = "STDOUT äöü"
    stdFileMap(Stdout).write(string, Encoding)
    assert(spoolerTask.invokeGet("stdout_text") == string)
  }

  "stderr_text" in {
    val string = "STDERR ÄÖÜ"
    stdFileMap(Stderr).write(string, Encoding)
    assert(spoolerTask.invokeGet("stderr_text") == string)
  }

  for (unsupportedGetter ← List("create_subprocess", "priority", "priority_class")) {
    s"$unsupportedGetter is not supported" in {
      intercept[UnsupportedOperationException] { spoolerTask.invokeGet(unsupportedGetter) }
        .getMessage shouldEqual s"Universal Agent does not support method 'sos.spooler.Task.$unsupportedGetter'"
    }
  }
}
