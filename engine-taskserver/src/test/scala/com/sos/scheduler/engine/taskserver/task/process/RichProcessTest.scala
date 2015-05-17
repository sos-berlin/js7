package com.sos.scheduler.engine.taskserver.task.process

import com.sos.scheduler.engine.common.scalautil.Closers.implicits.RichClosersCloser
import com.sos.scheduler.engine.common.scalautil.Closers.withCloser
import com.sos.scheduler.engine.common.scalautil.FileUtils.implicits.RichPath
import com.sos.scheduler.engine.data.job.ReturnCode
import com.sos.scheduler.engine.taskserver.task.process.StdoutStderr.Stdout
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class RichProcessTest extends FreeSpec {

  "RichProcess" in {
    val envName = "ENVNAME"
    val envValue = "ENVVALUE"
    val echo = "FIRSTLINE"
    val exitCode = 42
    withCloser { closer ⇒
      val stdFileMap = RichProcess.createTemporaryStdFiles()
      closer.onClose { RichProcess.tryDeleteFiles(stdFileMap.values) }
      val shellProcess = RichProcess.startShellScript(name = "TEST", Map(envName → envValue), s"echo $echo\nexit $exitCode", stdFileMap)
      assert(shellProcess.files.size == 3)
      val returnCode = shellProcess.waitForTermination()
      assert(returnCode == ReturnCode(exitCode))
      assert(!shellProcess.closed.isCompleted)
      shellProcess.close()
      assert(shellProcess.closed.isCompleted)
      assert(stdFileMap(Stdout).contentString contains echo)
    }
  }
}
