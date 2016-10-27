package com.sos.scheduler.engine.data.filebased

import com.sos.scheduler.engine.base.sprayjson.typed.{Subtype, TypedJsonFormat}
import com.sos.scheduler.engine.data.event.Event
import spray.json.DefaultJsonProtocol._

/**
  * Event related to a FileBased, this is a JobScheduler object which may be configured in a monitored (XML) file.
  *
  * Such an event denotes a FileBase (an JobScheduler object) itself, not any configuration file of the object.
  *
  * @author Joacim Zschimmer
  */
sealed trait FileBasedEvent
extends Event {
  type Key = TypedPath
}

/**
  * The FileBased has been activated and become useable.
  */
case object FileBasedActivated
extends FileBasedEvent

sealed trait FileBasedAddedOrReplaced
extends FileBasedEvent

/**
  * The FileBased has been added to the running JobScheduler.
  */
case object FileBasedAdded
extends FileBasedAddedOrReplaced

/**
  * The FileBased has been replaced.
  */
case object FileBasedReplaced
extends FileBasedAddedOrReplaced

/**
  * The FileBased has been removed.
  */
case object FileBasedRemoved
extends FileBasedEvent

object FileBasedEvent {
  implicit val jsonFormat = TypedJsonFormat[FileBasedEvent](
    Subtype(jsonFormat0(() ⇒ FileBasedActivated)),
    Subtype(jsonFormat0(() ⇒ FileBasedAdded)),
    Subtype(jsonFormat0(() ⇒ FileBasedReplaced)),
    Subtype(jsonFormat0(() ⇒ FileBasedRemoved)))
}
