package com.sos.scheduler.engine.data.job

final case class TaskStarted(taskId: TaskId, jobPath: JobPath)
extends TaskEvent
