package com.sos.scheduler.engine.data.scheduler

import com.sos.scheduler.engine.data.base.IsString

final case class SchedulerId(string: String) extends IsString


object SchedulerId extends IsString.HasJsonFormat[SchedulerId]
