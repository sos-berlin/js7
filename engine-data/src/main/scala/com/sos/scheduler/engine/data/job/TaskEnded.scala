package com.sos.scheduler.engine.data.job

final case class TaskEnded(taskId: TaskId, jobPath: JobPath, returnCode: ReturnCode)
extends TaskEvent
