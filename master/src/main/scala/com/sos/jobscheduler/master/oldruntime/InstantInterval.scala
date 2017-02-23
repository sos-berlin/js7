package com.sos.jobscheduler.master.oldruntime

import com.sos.jobscheduler.common.time.ScalaTime._
import java.time._
import scala.language.implicitConversions

/**
  * @author Joacim Zschimmer
  */
final case class InstantInterval(from: Instant, until: Instant)

object InstantInterval {
  def apply(from: Instant, duration: Duration): InstantInterval =
    new InstantInterval(from, from + duration)

  implicit def apply(pair: (Instant, Instant)): InstantInterval =
    new InstantInterval(pair._1, pair._2)
}
