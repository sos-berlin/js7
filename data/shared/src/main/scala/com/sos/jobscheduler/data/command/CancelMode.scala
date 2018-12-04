package com.sos.jobscheduler.data.command

import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}

/**
  * @author Joacim Zschimmer
  */
sealed trait CancelMode

object CancelMode
{
  val Default = NotStarted

  case object NotStarted extends CancelMode
  case object FreshOrStarted extends CancelMode

  implicit val jsonCodec = TypedJsonCodec[CancelMode](
    Subtype(NotStarted),
    Subtype(FreshOrStarted))
}
