package js7.journal.data

import java.nio.file.Path
import js7.base.circeutils.typed.TypedJsonCodec
import js7.data.event.SnapshotableState

/** Directory and filename prefix of a journal.
  * <p>
  * JVM-only methods are implemented in [[js7.journal.files.JournalFiles.extensions]].
  */
final case class JournalLocation(
  S: SnapshotableState.HasCodec,
  /** Path without extension, like "/directory/test". */
  fileBase: Path):

  val name: String = fileBase.getFileName.toString

  def snapshotObjectJsonCodec: TypedJsonCodec[Any] =
    S.snapshotObjectJsonCodec


object JournalLocation:
  private val TmpSuffix = ".tmp"

  def toTemporaryFile(file: Path): Path =
    file.resolveSibling(s"${file.getFileName}$TmpSuffix")
