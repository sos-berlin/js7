package com.sos.jobscheduler.core.common.jsonseq

/**
  * @author Joacim Zschimmer
  */
final case class PositionAnd[A](position: Long, value: A)
