package com.sos.jobscheduler.base.utils

trait HasKey {
  type Key
  def key: Key
}
