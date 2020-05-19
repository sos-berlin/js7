package com.sos.jobscheduler.proxy

import com.sos.jobscheduler.data.event.NoKeyEvent

sealed trait ProxyEvent extends NoKeyEvent

object ProxyEvent
{
  case object ProxyStarted extends ProxyEvent
}
