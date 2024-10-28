package js7.core.item

import java.io.File.separator
import java.nio.file.Path
import js7.base.problem.Checked.*
import js7.base.problem.{Checked, Problem}
import js7.base.utils.Assertions.assertThat
import js7.data.item.{InventoryItemPath, SourceType, VersionedItemPath}

/**
  * @author Joacim Zschimmer
  */
object ItemPaths:
  def fileToItemPath[P <: InventoryItemPath](
    companions: Iterable[InventoryItemPath.Companion[P]],
    directory: Path,
    file: Path)
  : Checked[P] =
    fileToItemPathAndSourceType(companions, directory, file)
      .map(_._1.asInstanceOf[P])

  def fileToVersionedItemPath(
    companions: Iterable[VersionedItemPath.AnyCompanion],
    directory: Path,
    file: Path)
  : Checked[VersionedItemPath] =
    fileToItemPathAndSourceType(companions, directory, file)
      .map(_._1.asInstanceOf[VersionedItemPath])

  def fileToItemPathAndSourceType(companions: Iterable[InventoryItemPath.AnyCompanion], directory: Path, file: Path): Checked[(js7.data.item.InventoryItemPath, SourceType)] =
    assertThat(file.startsWith(directory))
    val relativePath = file.subpath(directory.getNameCount, file.getNameCount)
    val string = fileToString(relativePath)
    companions.iterator
      .map(_.fromFile(string))
      .collectFirst { case Some(o) => o }
      .toChecked(AlienFileProblem(relativePath, companions.toSet))
      .flatten

  private def fileToString(file: Path): String =
    file.toString.replace(file.getFileSystem.getSeparator.charAt(0), '/')

  final case class AlienFileProblem(relativePath: Path, companions: Set[InventoryItemPath.AnyCompanion])
  extends Problem.Lazy(
    "File '..." + separator + relativePath.toString.stripPrefix(separator) + "'" +
      " is not recognized as a configuration file" +
      " (like " +
      companions.flatMap(_.sourceTypeToFilenameExtension.values).map("*" + _).mkString(", ") +
      ")")
