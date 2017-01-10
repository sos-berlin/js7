package com.sos.scheduler.engine.data.event.custom

import com.sos.scheduler.engine.base.generic.IsString
import com.sos.scheduler.engine.base.sprayjson.typed.{Subtype, TypedJsonFormat}
import com.sos.scheduler.engine.data.event.Event

/**
  * @author Joacim Zschimmer
  */
trait CustomEvent extends Event {
  type Key = CustomEvent.Key
}

object CustomEvent {
  final case class Key(string: String) extends IsString
  object Key extends IsString.Companion[Key]

  implicit val OrderEventJsonFormat = TypedJsonFormat[CustomEvent](
    Subtype[VariablesCustomEvent])
}
