package com.sos.jobscheduler.core.common.jsonseq

/**
  * @author Joacim Zschimmer
  */
final case class PositionAnd[@specialized(Long/*EventId*/) A](position: Long, value: A)
