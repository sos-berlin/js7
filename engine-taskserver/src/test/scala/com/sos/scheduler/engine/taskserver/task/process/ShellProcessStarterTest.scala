package com.sos.scheduler.engine.taskserver.task.process

import com.sos.scheduler.engine.data.job.ResultCode
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner
import scala.collection.mutable

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class ShellProcessStarterTest extends FreeSpec {
  "ShellProcessStarter and ShellProcess" in {
    val envName = "ENVNAME"
    val envValue = "ENVVALUE"
    val firstLine = "FIRSTLINE"
    val exitCode = 42
    val shellProcess = ShellProcessStarter.start(name = "TEST", Map(envName → envValue), s"echo $firstLine\nexit $exitCode")
    assert(shellProcess.files.size == 3)
    val outputLines = mutable.Buffer[String]()
    val resultCode = shellProcess.waitForTermination { line ⇒ outputLines += line }
    assert(resultCode == ResultCode(exitCode))
    assert(shellProcess.firstStdoutLine contains firstLine)
    assert(!shellProcess.closedFuture.isCompleted)
    shellProcess.close()
    assert(shellProcess.closedFuture.isCompleted)
  }
}
