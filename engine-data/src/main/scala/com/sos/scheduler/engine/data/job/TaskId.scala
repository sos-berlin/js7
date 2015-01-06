package com.sos.scheduler.engine.data.job

import com.fasterxml.jackson.annotation.JsonCreator
import com.sos.scheduler.engine.cplusplus.runtime.annotation.ForCpp
import com.sos.scheduler.engine.data.base.GenericInt
import scala.annotation.meta.getter

@ForCpp
final case class TaskId(@(ForCpp @getter) value: Int) extends GenericInt {

  override def toString = s"TaskId $value"

  def string = value.toString
}

object TaskId {
  @JsonCreator def jsonCreator(taskId: Int) =
    new TaskId(taskId)
}
