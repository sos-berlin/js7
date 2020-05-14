package com.sos.jobscheduler.base.eventbus

trait EventPublisher[Event]
{
  def publish(event: Event): Unit
}
