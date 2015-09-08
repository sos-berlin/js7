package com.sos.scheduler.engine.taskserver.task.process

import com.sos.scheduler.engine.common.system.OperatingSystem.isWindows
import com.sos.scheduler.engine.taskserver.task.process.Processes.{Pid, processToPidOption}
import java.lang.ProcessBuilder.Redirect.PIPE
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class ProcessesTest extends FreeSpec {

  "processToPidOption" in {
    if (isWindows) {
      val process = new ProcessBuilder(sys.env("ComSpec"), "/C", "rem").start()
      assert(processToPidOption(process).isEmpty)
      process.waitFor()
    } else {
      val process = new ProcessBuilder("/bin/sh", "-c", "echo $BASHPID").redirectInput(PIPE).start()
      val echoLine = io.Source.fromInputStream(process.getInputStream).getLines.next
      assert(processToPidOption(process) contains Pid(echoLine.toLong))
      process.waitFor()
    }
  }
}
