package com.sos.scheduler.engine.taskserver.task

import com.sos.scheduler.engine.taskserver.task.StdFiles.prefixLinesWithStdoutOrStderr
import com.sos.scheduler.engine.taskserver.task.process.StdoutStderr.Stdout
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class StdFilesTest extends FreeSpec {

  "prefixLines" in {
    assert(prefixLinesWithStdoutOrStderr(Stdout, "") == "[stdout] ")
    assert(prefixLinesWithStdoutOrStderr(Stdout, "a") == "[stdout] a")
    assert(prefixLinesWithStdoutOrStderr(Stdout, "a\n") == "[stdout] a")
    assert(prefixLinesWithStdoutOrStderr(Stdout, "a\nb") == "[stdout] a\n[stdout] b")
  }
}
