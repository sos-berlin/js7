package com.sos.scheduler.engine.data.filebased

import com.sos.scheduler.engine.base.sprayjson.typed.{Subtype, TypedJsonFormat}
import com.sos.scheduler.engine.data.event.Event
import spray.json.DefaultJsonProtocol._

/**
  * @author Joacim Zschimmer
  */
sealed trait FileBasedEvent
extends Event {
  type Key = TypedPath
}

case object FileBasedActivated
extends FileBasedEvent

sealed trait FileBasedAddedOrReplaced
extends FileBasedEvent

case object FileBasedAdded
extends FileBasedAddedOrReplaced

case object FileBasedReplaced
extends FileBasedAddedOrReplaced

case object FileBasedRemoved
extends FileBasedEvent

object FileBasedEvent {
  implicit val jsonFormat = TypedJsonFormat[FileBasedEvent](
    Subtype(jsonFormat0(() ⇒ FileBasedActivated)),
    Subtype(jsonFormat0(() ⇒ FileBasedAdded)),
    Subtype(jsonFormat0(() ⇒ FileBasedReplaced)),
    Subtype(jsonFormat0(() ⇒ FileBasedRemoved)))
}
