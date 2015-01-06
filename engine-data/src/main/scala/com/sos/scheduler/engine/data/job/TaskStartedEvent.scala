package com.sos.scheduler.engine.data.job

final case class TaskStartedEvent(taskId: TaskId, jobPath: JobPath)
extends TaskEvent
