package com.sos.jobscheduler.data.event

import com.sos.jobscheduler.base.circeutils.CirceUtils.singletonCodec
import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import io.circe.Json

sealed trait JournalEvent extends NoKeyEvent

object JournalEvent
{
  sealed trait SnapshotTaken extends JournalEvent
  case object SnapshotTaken extends SnapshotTaken {
    val TypeName = "SnapshotTaken"
    val TypeNameJson = Json.fromString(TypeName)
  }

  implicit val jsonCodec = TypedJsonCodec[JournalEvent](
    Subtype.named(singletonCodec(SnapshotTaken), SnapshotTaken.TypeName))
}
