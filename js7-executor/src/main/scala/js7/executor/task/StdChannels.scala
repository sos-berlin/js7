package js7.executor.task

import java.io.Writer

/**
  * @author Joacim Zschimmer
  */
final class StdChannels(val charBufferSize: Int, val stdoutWriter: Writer, val stderrWriter: Writer)
