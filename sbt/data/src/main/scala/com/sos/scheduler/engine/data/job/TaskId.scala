package com.sos.scheduler.engine.data.job

import com.sos.scheduler.engine.base.generic.GenericInt
import spray.json.{JsNumber, JsString, JsValue, JsonFormat}

final case class TaskId(number: Int) extends GenericInt {

  override def toString = s"TaskId $number"

  def +(n: Int) = TaskId(number + n)

  def -(n: Int) = TaskId(number - n)

  def string = number.toString
}

object TaskId extends GenericInt.Companion[TaskId] {
  val Null = TaskId(0)
  // TaskId(1) is not used
  val SchedulerStart = TaskId(2)  // Misused for JobScheduler start database record. This number only with a new database.
  val First = TaskId(3)  // TaskId of the first Task with a new database

  implicit object MyJsonFormat extends JsonFormat[TaskId] {
    def read(jsValue: JsValue): TaskId = jsValue match {
      case JsString(string) ⇒ TaskId(string.toInt)
      case JsNumber(number) ⇒ TaskId(number.toInt)
      case _ ⇒ sys.error(s"String expected instead of ${jsValue.getClass.getSimpleName}")
    }

    def write(o: TaskId) = JsString(o.string)
  }
}
