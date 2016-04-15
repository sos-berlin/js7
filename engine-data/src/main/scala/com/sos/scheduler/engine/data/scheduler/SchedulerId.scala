package com.sos.scheduler.engine.data.scheduler

import com.sos.scheduler.engine.base.generic.IsString

final case class SchedulerId(string: String) extends IsString


object SchedulerId extends IsString.HasJsonFormat[SchedulerId]
