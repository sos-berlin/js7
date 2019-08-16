package com.sos.jobscheduler.core.configuration

import scala.concurrent.duration.FiniteDuration

final case class EventFetcherConf(
  eventFetchTimeout: FiniteDuration)
