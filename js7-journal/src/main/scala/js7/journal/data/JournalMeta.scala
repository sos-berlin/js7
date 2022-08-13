package js7.journal.data

import java.nio.file.Path
import js7.base.circeutils.typed.TypedJsonCodec
import js7.data.event.{Event, KeyedEventTypedJsonCodec, SnapshotableState}

/**
  * @author Joacim Zschimmer
  */
final case class JournalMeta(
  snapshotObjectJsonCodec: TypedJsonCodec[Any],
  eventJsonCodec: KeyedEventTypedJsonCodec[Event],
  /** Path without extension, like "/directory/test". */
  fileBase: Path)
{
  implicit val implicitEventJsonCodec: KeyedEventTypedJsonCodec[Event] = eventJsonCodec

  val name = fileBase.getFileName.toString
}

object JournalMeta
{
  def apply[S <: SnapshotableState[S]](companion: SnapshotableState.Companion[S], fileBase: Path) =
    new JournalMeta(companion.snapshotObjectJsonCodec, companion.keyedEventJsonCodec, fileBase)
}
