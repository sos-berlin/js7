package com.sos.scheduler.engine.taskserver.task.process

import com.sos.scheduler.engine.common.scalautil.FileUtils.implicits.RichPath
import com.sos.scheduler.engine.common.system.OperatingSystem.isWindows
import com.sos.scheduler.engine.taskserver.task.process.Processes._
import com.sos.scheduler.engine.taskserver.task.process.StdoutStderr.Stdout
import java.lang.ProcessBuilder.Redirect.PIPE
import java.nio.file.Files.{delete, exists}
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner
import scala.collection.JavaConversions._

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class ProcessesTest extends FreeSpec {

  "processToPidOption, toShellCommandArguments" in {
    if (isWindows) {
      val process = new ProcessBuilder(toShellCommandArguments("rem")).start()
      assert(processToPidOption(process).isEmpty)
      process.waitFor()
    } else {
      val process = new ProcessBuilder(toShellCommandArguments("echo $$")).redirectInput(PIPE).start()
      val echoLine = io.Source.fromInputStream(process.getInputStream).getLines.next
      assert(processToPidOption(process) contains Pid(echoLine.toLong))
      process.waitFor()
    }
  }

  "newTemporaryShellFile returns executable file, toShellCommandArguments" in {
    val file = newTemporaryShellFile("NAME")
    assert(exists(file))
    assert(!(file.toString contains "--"))
    file.contentString = "echo HELLO TEST\n"
    val process = new ProcessBuilder(toShellCommandArguments(file)).redirectOutput(PIPE).start()
    io.Source.fromInputStream(process.getInputStream).getLines exists { _ contains "HELLO TEST" }
    process.waitFor()
    delete(file)
  }

  "newTemporaryOutputFile" in {
    val file = newTemporaryOutputFile("NAME", Stdout)
    assert(exists(file))
    assert(!(file.toString contains "--"))
    delete(file)
  }
}
