package com.sos.scheduler.engine.data.log

import com.sos.scheduler.engine.base.sprayjson.typed.{SimpleTypedJsonFormat, TypedJsonFormat}
import com.sos.scheduler.engine.data.event.NoKeyEvent
import com.sos.scheduler.engine.data.log.LogEvent._
import com.sos.scheduler.engine.data.message.MessageCode
import scala.PartialFunction.condOpt
import spray.json.DefaultJsonProtocol._
import spray.json._

abstract sealed class LogEvent extends NoKeyEvent {

  def level: SchedulerLogLevel
  def message: String

  final def codeOption: Option[MessageCode] = messageToCode(message)
}

sealed trait InfoOrHigherLogged extends LogEvent

final case class InfoLogEvent(message: String) extends InfoOrHigherLogged {
  def level = SchedulerLogLevel.info
}

sealed trait WarningOrHigherLogged extends InfoOrHigherLogged

final case class WarningLogEvent(message: String) extends WarningOrHigherLogged {
  def level = SchedulerLogLevel.warning
}

final case class ErrorLogEvent(message: String) extends WarningOrHigherLogged {
  def level = SchedulerLogLevel.error
}

private case class OtherLogEvent(level: SchedulerLogLevel, message: String)
extends LogEvent

object LogEvent {
  private val CodeRegex = "^([A-Z]+(-[0-9A-Z]+)+)".r.unanchored

  def messageToCode(message: String): Option[MessageCode] =
    condOpt(message) {
      case CodeRegex(code, _) ⇒ new MessageCode(code)
    }

  def apply(level: SchedulerLogLevel, line: String): LogEvent =
    level match {
      case SchedulerLogLevel.info ⇒ InfoLogEvent(line)
      case SchedulerLogLevel.warning ⇒ WarningLogEvent(line)
      case SchedulerLogLevel.error ⇒ ErrorLogEvent(line)
      case _ ⇒ OtherLogEvent(level, line)
    }

  implicit object LogEventJsonFormat extends SimpleTypedJsonFormat[LogEvent] {
    protected def typeField = TypedJsonFormat.DefaultTypeFieldName → JsString("Logged")

    protected val subclasses = Set[Class[_ <: LogEvent]](
      classOf[InfoLogEvent],
      classOf[WarningLogEvent],
      classOf[ErrorLogEvent],
      classOf[OtherLogEvent])

    private implicit def levelJsonFormat = SchedulerLogLevel.MyJsonFormat
    private val transferJsonFormat = jsonFormat2(OtherLogEvent)  // Here, we (mis)use OtherLogEvent as a transfer class

    protected def typelessWrite(o: LogEvent) =
      transferJsonFormat.write(OtherLogEvent(o.level, o.message)).asJsObject

    def read(jsValue: JsValue) = {
      val OtherLogEvent(level, message) = transferJsonFormat.read(jsValue)
      LogEvent(level, message)
    }
  }
}
