package com.sos.scheduler.engine.data.event

trait KeyedEvent extends Event {
  type Key
  def key: Key
}
