package com.sos.scheduler.engine.data.job

import com.fasterxml.jackson.annotation.JsonCreator
import com.sos.scheduler.engine.base.generic.GenericInt
import com.sos.scheduler.engine.cplusplus.runtime.annotation.ForCpp
import scala.annotation.meta.getter
import spray.json.{JsNumber, JsString, JsValue, JsonFormat}

@ForCpp
final case class TaskId(@(ForCpp @getter) value: Int) extends GenericInt {

  override def toString = s"TaskId $value"

  def +(n: Int) = TaskId(value + n)

  def -(n: Int) = TaskId(value - n)

  def string = value.toString
}

object TaskId extends GenericInt.Companion[TaskId] {
  val Null = TaskId(0)
  // TaskId(1) is not used
  val SchedulerStart = TaskId(2)  // Misused for JobScheduler start database record. This number only with a new database.
  val First = TaskId(3)  // TaskId of the first Task with a new database

  @JsonCreator def jsonCreator(taskId: Int) = new TaskId(taskId)

  implicit object MyJsonFormat extends JsonFormat[TaskId] {
    def read(jsValue: JsValue): TaskId = jsValue match {
      case JsString(string) ⇒ TaskId(string.toInt)
      case JsNumber(number) ⇒ TaskId(number.toInt)
      case _ ⇒ sys.error(s"String expected instead of ${jsValue.getClass.getSimpleName}")
    }

    def write(o: TaskId) = JsString(o.string)
  }
}
