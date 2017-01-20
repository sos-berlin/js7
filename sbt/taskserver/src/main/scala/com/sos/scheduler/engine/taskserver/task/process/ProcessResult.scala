package com.sos.scheduler.engine.taskserver.task.process

import com.sos.scheduler.engine.data.job.ReturnCode

/**
 * @author Joacim Zschimmer
 */
final case class ProcessResult(returnCode: ReturnCode, stdoutString: String, stderrString: String)
