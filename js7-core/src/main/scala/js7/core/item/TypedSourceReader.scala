package js7.core.item

import java.nio.file.Path
import js7.base.problem.Checked._
import js7.base.problem.{Checked, Problem}
import js7.base.utils.Collections.implicits.RichTraversable
import js7.common.files.DirectoryReader
import js7.common.scalautil.FileUtils.syntax.RichPath
import js7.data.item.{InventoryItem, ItemPath}

/**
  * @author Joacim Zschimmer
  */
final class TypedSourceReader(directory: Path, readers: Iterable[InventoryItemReader])
{
  private val companionToReader: Map[ItemPath.AnyCompanion, InventoryItemReader] = readers.toKeyedMap(_.itemPathCompanion)
  private val itemPathCompanions = readers.map(_.companion.itemPathCompanion)

  // For tests
  def readCompleteDirectory(): Checked[Seq[InventoryItem]] =
    readInventoryItems(DirectoryReader.files(directory))

  def readInventoryItems(files: Seq[Path]): Checked[Seq[InventoryItem]] = {
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

  private def toCheckedItem(o: ItemSource): Checked[InventoryItem] =
    companionToReader(o.path.companion).readUntyped(o.path, o.byteArray, o.sourceType)
}
