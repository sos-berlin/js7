package js7.journal.data

import java.nio.file.Path
import js7.data.event.SnapshotableState

final case class JournalLocation(
  S: SnapshotableState.HasCodec,
  /** Path without extension, like "/directory/test". */
  fileBase: Path)
{
  val name = fileBase.getFileName.toString

  def snapshotObjectJsonCodec = S.snapshotObjectJsonCodec
}
