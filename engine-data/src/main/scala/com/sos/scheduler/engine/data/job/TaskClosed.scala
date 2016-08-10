package com.sos.scheduler.engine.data.job

/** Nach [[com.sos.scheduler.engine.data.job.TaskEnded]] und nachdem das Task-Objekt geschlossen worden ist. */
final case class TaskClosed(taskId: TaskId, jobPath: JobPath)
extends TaskEvent
