package com.sos.scheduler.engine.data.scheduler

import com.sos.scheduler.engine.base.generic.IsString

/**
  * @author Joacim Zschimmer
  */
final case class SupervisorUri(string: String) extends IsString

object SupervisorUri extends IsString.Companion[SupervisorUri]
