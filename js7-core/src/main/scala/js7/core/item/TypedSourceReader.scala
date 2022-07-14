package js7.core.item

import java.nio.file.Path
import js7.base.io.file.FileUtils.syntax.RichPath
import js7.base.problem.Checked.*
import js7.base.problem.{Checked, Problem}
import js7.base.utils.Collections.implicits.RichIterable
import js7.common.files.DirectoryReader
import js7.data.item.{InventoryItem, InventoryItemKey, InventoryItemPath, VersionId, VersionedItemPath}

/**
  * @author Joacim Zschimmer
  */
final class TypedSourceReader(directory: Path, readers: Iterable[ItemReader])
{
  private val companionToReader: Map[InventoryItemPath.AnyCompanion, ItemReader] =
    readers.toKeyedMap(_.itemPathCompanion)

  private val itemPathCompanions: Seq[InventoryItemPath.AnyCompanion] =
    readers.map(_.companion.Path).toSeq

  // For tests
  def readCompleteDirectory(): Checked[Seq[InventoryItem]] =
    readItems(DirectoryReader.files(directory))

  def readItems(files: Seq[Path]): Checked[Seq[InventoryItem]] = {
    val checkedTypedFiles = files.map(toTypedFile)
    InventoryItemFile.checkUniqueness(checkedTypedFiles collect { case Right(o) => o }) match {
      case Left(problem) =>
        Left(Problem.Combined(checkedTypedFiles.collect { case Left(p) => p } :+ problem))
      case Right(_) =>
        checkedTypedFiles.traverseAndCombineProblems(typedFile => toCheckedItem(readTypedSource(typedFile)))
    }
  }

  private def toTypedFile(file: Path): Checked[InventoryItemFile] =
    InventoryItemFile.checked(directory, file, itemPathCompanions)

  private def readTypedSource(itemFile: InventoryItemFile): ItemSource =
    ItemSource(itemFile.file.byteArray, itemFile.path, itemFile.sourceType)

  private def toCheckedItem(o: ItemSource): Checked[InventoryItem] = {
    val key: InventoryItemKey = o.path match {
      case path: VersionedItemPath => path ~ VersionId.Anonymous
      case key: InventoryItemKey => key
    }
    val itemReader = companionToReader(o.path.companion)
    itemReader.readUntyped(key.asInstanceOf[itemReader.companion.Key], o.byteArray, o.sourceType)
  }
}
