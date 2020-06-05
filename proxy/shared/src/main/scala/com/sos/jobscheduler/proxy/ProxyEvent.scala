package js7.proxy

import js7.data.event.NoKeyEvent

sealed trait ProxyEvent extends NoKeyEvent

object ProxyEvent
{
  case object ProxyStarted extends ProxyEvent
}
