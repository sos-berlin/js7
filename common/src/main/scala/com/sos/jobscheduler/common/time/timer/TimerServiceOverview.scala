package com.sos.jobscheduler.common.time.timer

import io.circe.generic.JsonCodec

/**
  * @author Joacim Zschimmer
  */
@JsonCodec
final case class TimerServiceOverview(
  count: Int,
  completeCount: Int,
  wakeCount: Int,
  prematureWakeCount: Option[Int] = None,
  first: Option[TimerOverview] = None,
  last: Option[TimerOverview] = None
)
