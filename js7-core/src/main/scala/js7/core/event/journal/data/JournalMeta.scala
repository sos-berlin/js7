package js7.core.event.journal.data

import java.nio.file.Path
import js7.base.circeutils.typed.TypedJsonCodec
import js7.data.event.{Event, KeyedEventTypedJsonCodec}

/**
  * @author Joacim Zschimmer
  */
final case class JournalMeta(
  snapshotJsonCodec: TypedJsonCodec[Any],
  implicit val eventJsonCodec: KeyedEventTypedJsonCodec[Event],
  /** Path without extension, like "/directory/test". */
  fileBase: Path)
{
  val name = fileBase.getFileName.toString
}
