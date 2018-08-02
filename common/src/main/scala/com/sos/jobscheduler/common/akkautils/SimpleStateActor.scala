package com.sos.jobscheduler.common.akkautils

import akka.actor.Actor

/**
  * @author Joacim Zschimmer
  */
trait SimpleStateActor extends Actor
{
  protected def become(state: String)(recv: Receive): Unit
}
