package com.sos.scheduler.engine.data.filebased

import com.sos.scheduler.engine.data.event.KeyedEvent

trait FileBasedEvent extends KeyedEvent {
  type Key = TypedPath
  def key = typedPath

  def typedPath: TypedPath
}
