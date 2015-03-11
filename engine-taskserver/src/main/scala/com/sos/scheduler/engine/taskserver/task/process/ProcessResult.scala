package com.sos.scheduler.engine.taskserver.task.process

import com.sos.scheduler.engine.data.job.ResultCode

/**
 * @author Joacim Zschimmer
 */
final case class ProcessResult(resultCode: ResultCode, stdoutString: String, stderrString: String)
