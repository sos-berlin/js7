package com.sos.jobscheduler.taskserver.task.process

import com.sos.jobscheduler.data.system.StdoutStderr.StdoutStderrType

/**
  * @author Joacim Zschimmer
  */
trait StdoutStderrWriter {

  def chunkSize: Int

  def writeChunk(t: StdoutStderrType, chunk: String): Unit
}
