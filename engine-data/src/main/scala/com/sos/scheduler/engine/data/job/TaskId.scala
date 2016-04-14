package com.sos.scheduler.engine.data.job

import com.fasterxml.jackson.annotation.JsonCreator
import com.sos.scheduler.engine.base.generic.GenericInt
import com.sos.scheduler.engine.cplusplus.runtime.annotation.ForCpp
import scala.annotation.meta.getter
import spray.json.{JsNumber, JsString, JsValue, JsonFormat}

@ForCpp
final case class TaskId(@(ForCpp @getter) value: Int) extends GenericInt {

  override def toString = s"TaskId $value"

  def string = value.toString
}

object TaskId {
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
