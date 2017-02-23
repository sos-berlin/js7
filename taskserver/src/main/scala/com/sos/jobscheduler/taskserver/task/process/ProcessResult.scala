package com.sos.jobscheduler.taskserver.task.process

import com.sos.jobscheduler.data.job.ReturnCode

/**
 * @author Joacim Zschimmer
 */
final case class ProcessResult(returnCode: ReturnCode, stdoutString: String, stderrString: String)
