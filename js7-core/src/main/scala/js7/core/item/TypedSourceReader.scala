package js7.core.item

import java.nio.file.Path
import js7.base.io.file.FileUtils.syntax.RichPath
import js7.base.problem.Checked._
import js7.base.problem.{Checked, Problem}
import js7.base.utils.Collections.implicits.RichIterable
import js7.common.files.DirectoryReader
import js7.data.item.{ItemPath, VersionId, VersionedItem}

/**
  * @author Joacim Zschimmer
  */
final class TypedSourceReader(directory: Path, readers: Iterable[VersionedItemReader])
{
  private val companionToReader: Map[ItemPath.AnyCompanion, VersionedItemReader] = readers.toKeyedMap(_.itemPathCompanion)
  private val itemPathCompanions = readers.map(_.companion.Path)

  // For tests
  def readCompleteDirectory(): Checked[Seq[VersionedItem]] =
    readVersionedItems(DirectoryReader.files(directory))

  def readVersionedItems(files: Seq[Path]): Checked[Seq[VersionedItem]] = {
    val checkedTypedFiles = files.map(toTypedFile)
    VersionedItemFile.checkUniqueness(checkedTypedFiles collect { case Right(o) => o }) match {
      case Left(problem) =>
        Left(Problem.Combined(checkedTypedFiles.collect { case Left(p) => p } :+ problem))
      case Right(_) =>
        checkedTypedFiles.traverseAndCombineProblems(typedFile => toCheckedItem(readTypedSource(typedFile)))
    }
  }

  private def toTypedFile(file: Path): Checked[VersionedItemFile] =
    VersionedItemFile.checked(directory, file, itemPathCompanions)

  private def readTypedSource(itemFile: VersionedItemFile): ItemSource =
    ItemSource(itemFile.file.byteArray, itemFile.path, itemFile.sourceType)

  private def toCheckedItem(o: ItemSource): Checked[VersionedItem] = {
    val id = o.path ~ VersionId.Anonymous
    companionToReader(o.path.companion).readUntyped(id, o.byteArray, o.sourceType)
  }
}
