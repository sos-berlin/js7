package com.sos.scheduler.engine.base.utils

trait HasKey {
  type Key
  def key: Key
}
