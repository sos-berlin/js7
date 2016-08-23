package com.sos.scheduler.engine.data.filebased

import com.sos.scheduler.engine.data.event.Event

/**
  * @author Joacim Zschimmer
  */
sealed trait FileBasedEvent
extends Event { type Key = TypedPath }

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
