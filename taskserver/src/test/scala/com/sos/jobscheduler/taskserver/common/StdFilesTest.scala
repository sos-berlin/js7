package com.sos.jobscheduler.taskserver.common

import com.sos.jobscheduler.data.system.Stdout
import com.sos.jobscheduler.taskserver.common.StdFiles.prefixLinesWithStdoutOrStderr
import org.scalatest.FreeSpec

/**
 * @author Joacim Zschimmer
 */
final class StdFilesTest extends FreeSpec {

  "prefixLines" in {
    assert(prefixLinesWithStdoutOrStderr(Stdout, "") == "[stdout] ")
    assert(prefixLinesWithStdoutOrStderr(Stdout, "a") == "[stdout] a")
    assert(prefixLinesWithStdoutOrStderr(Stdout, "a\n") == "[stdout] a")
    assert(prefixLinesWithStdoutOrStderr(Stdout, "a\nb") == "[stdout] a\n[stdout] b")
  }
}
