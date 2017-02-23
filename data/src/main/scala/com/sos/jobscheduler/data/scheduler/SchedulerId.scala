package com.sos.jobscheduler.data.scheduler

import com.sos.jobscheduler.base.generic.IsString

final case class SchedulerId(string: String) extends IsString


object SchedulerId extends IsString.HasJsonFormat[SchedulerId]
