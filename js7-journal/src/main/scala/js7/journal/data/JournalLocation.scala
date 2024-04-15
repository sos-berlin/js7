package js7.journal.data

import java.nio.file.Path
import js7.base.circeutils.typed.TypedJsonCodec
import js7.data.event.SnapshotableState

final case class JournalLocation(
  S: SnapshotableState.HasCodec,
  /** Path without extension, like "/directory/test". */
  fileBase: Path):

  val name: String = fileBase.getFileName.toString

  def snapshotObjectJsonCodec: TypedJsonCodec[Any] =
    S.snapshotObjectJsonCodec
