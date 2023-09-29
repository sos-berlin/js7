package js7.core.item

import java.nio.file.Path
import js7.base.problem.{Checked, Problem}
import js7.core.item.ItemPaths.fileToItemPathAndSourceType
import js7.data.item.{InventoryItemPath, SourceType}

/**
  * @author Joacim Zschimmer
  */
final case class InventoryItemFile(file: Path, path: InventoryItemPath, sourceType: SourceType)

object InventoryItemFile:
  def checked(baseDirectory: Path, path: Path, companions: Iterable[js7.data.item.InventoryItemPath.AnyCompanion]): Checked[InventoryItemFile] =
    fileToItemPathAndSourceType(companions, baseDirectory, path)
      .map { case (itemPath, sourceType) => InventoryItemFile(path, itemPath, sourceType) }

  def checkUniqueness(typedFiles: Seq[InventoryItemFile]): Checked[typedFiles.type] =
    val duplicateFiles: Iterable[Path] =
      typedFiles.groupBy(_.path).filter(_._2.lengthIs >= 2).flatMap(_._2.map(_.file))
    if duplicateFiles.isEmpty then
      Right(typedFiles)
    else
      Left(Problem(s"Duplicate configuration files: ${duplicateFiles.toVector.sorted mkString ", "}"))
