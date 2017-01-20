package com.sos.scheduler.engine.taskserver.common

import com.sos.scheduler.engine.common.process.StdoutStderr.Stdout
import com.sos.scheduler.engine.taskserver.common.StdFiles.prefixLinesWithStdoutOrStderr
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
