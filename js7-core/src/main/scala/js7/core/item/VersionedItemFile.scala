package js7.core.item

import java.nio.file.Path
import js7.base.problem.{Checked, Problem}
import js7.core.item.ItemPaths.fileToItemPathAndSourceType
import js7.data.item.{SourceType, VersionedItemPath}

/**
  * @author Joacim Zschimmer
  */
final case class VersionedItemFile(file: Path, path: VersionedItemPath, sourceType: SourceType)

object VersionedItemFile
{
  def checked(baseDirectory: Path, path: Path, companions: Iterable[VersionedItemPath.AnyCompanion]): Checked[VersionedItemFile] =
    fileToItemPathAndSourceType(companions, baseDirectory, path)
      .map { case (itemPath, sourceType) => VersionedItemFile(path, itemPath, sourceType) }

  def checkUniqueness(typedFiles: Seq[VersionedItemFile]): Checked[typedFiles.type] = {
    val duplicateFiles: Iterable[Path] =
      typedFiles.groupBy(_.path).filter(_._2.lengthIs >= 2).flatMap(_._2.map(_.file))
    if (duplicateFiles.isEmpty)
      Right(typedFiles)
    else
      Left(Problem(s"Duplicate configuration files: ${duplicateFiles.toVector.sorted mkString ", "}"))
  }
}
