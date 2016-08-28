package com.sos.scheduler.engine.data.log

import com.sos.scheduler.engine.base.sprayjson.typed.{SimpleTypedJsonFormat, TypedJsonFormat}
import com.sos.scheduler.engine.data.event.NoKeyEvent
import com.sos.scheduler.engine.data.log.LogEvent._
import com.sos.scheduler.engine.data.message.MessageCode
import scala.PartialFunction.condOpt
import spray.json.DefaultJsonProtocol._
import spray.json._

sealed trait LogEvent extends NoKeyEvent {

  def level: SchedulerLogLevel
  def message: String

  final def codeOption: Option[MessageCode] = messageToCode(message)
}

sealed trait InfoOrHigherLogged extends LogEvent

final case class InfoLogged(message: String) extends InfoOrHigherLogged {
  def level = SchedulerLogLevel.info
}

sealed trait WarningOrHigherLogged extends InfoOrHigherLogged

final case class WarningLogged(message: String) extends WarningOrHigherLogged {
  def level = SchedulerLogLevel.warning
}

final case class ErrorLogged(message: String) extends WarningOrHigherLogged {
  def level = SchedulerLogLevel.error
}

private case class OtherLevelLogged(level: SchedulerLogLevel, message: String)
extends LogEvent

object LogEvent {
  private val CodeRegex = "^([A-Z]+(-[0-9A-Z]+)+)".r.unanchored

  def messageToCode(message: String): Option[MessageCode] =
    condOpt(message) {
      case CodeRegex(code, _) ⇒ new MessageCode(code)
    }

  def apply(level: SchedulerLogLevel, line: String): LogEvent =
    level match {
      case SchedulerLogLevel.info ⇒ InfoLogged(line)
      case SchedulerLogLevel.warning ⇒ WarningLogged(line)
      case SchedulerLogLevel.error ⇒ ErrorLogged(line)
      case _ ⇒ OtherLevelLogged(level, line)
    }

  implicit object LogEventJsonFormat extends SimpleTypedJsonFormat[LogEvent] {
    protected def typeField = TypedJsonFormat.DefaultTypeFieldName → JsString("Logged")

    protected val subclasses = Set[Class[_ <: LogEvent]](
      classOf[InfoLogged],
      classOf[WarningLogged],
      classOf[ErrorLogged],
      classOf[OtherLevelLogged])

    private implicit def levelJsonFormat = SchedulerLogLevel.MyJsonFormat
    private val transferJsonFormat = jsonFormat2(OtherLevelLogged)  // Here, we (mis)use OtherLevelLogged as a transfer class

    protected def typelessWrite(o: LogEvent) =
      transferJsonFormat.write(OtherLevelLogged(o.level, o.message)).asJsObject

    def read(jsValue: JsValue) = {
      val OtherLevelLogged(level, message) = transferJsonFormat.read(jsValue)
      LogEvent(level, message)
    }
  }
}
