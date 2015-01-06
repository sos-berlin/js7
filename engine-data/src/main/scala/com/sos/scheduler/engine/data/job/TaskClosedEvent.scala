package com.sos.scheduler.engine.data.job

/** Nach [[com.sos.scheduler.engine.data.job.TaskEndedEvent]] und nachdem das Task-Objekt geschlossen worden ist. */
final case class TaskClosedEvent(taskId: TaskId, jobPath: JobPath)
extends TaskEvent