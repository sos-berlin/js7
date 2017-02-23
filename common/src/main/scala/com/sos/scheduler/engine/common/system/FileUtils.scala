package com.sos.scheduler.engine.common.system

import java.nio.file.Paths

/**
  * @author Joacim Zschimmer
  */
object FileUtils {
  def temporaryDirectory = Paths get sys.props("java.io.tmpdir")
}
