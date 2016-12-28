package com.sos.scheduler.engine.base.generic

/**
  * May be used for Future[Completed].
  * Like Akka's `Done`.
  *
  * @author Joacim Zschimmer
  */
sealed trait Completed

/**
  * May be used for Future[Completed].
  * Like Akka's `Done`.
  */
object Completed extends Completed
