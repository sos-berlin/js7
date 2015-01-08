package com.sos.scheduler.engine.data.job

import com.sos.scheduler.engine.data.event.{AbstractEvent, KeyedEvent}

trait TaskEvent extends AbstractEvent with KeyedEvent {
  type Key = TaskId
  final def key = taskId

  def taskId: TaskId
  def jobPath: JobPath
}
