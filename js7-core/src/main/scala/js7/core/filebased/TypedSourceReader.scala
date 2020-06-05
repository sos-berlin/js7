package js7.core.filebased

import java.nio.file.Path
import js7.base.problem.Checked._
import js7.base.problem.{Checked, Problem}
import js7.base.utils.Collections.implicits.RichTraversable
import js7.common.files.DirectoryReader
import js7.common.scalautil.FileUtils.syntax.RichPath
import js7.data.filebased.{FileBased, TypedPath}

/**
  * @author Joacim Zschimmer
  */
final class TypedSourceReader(directory: Path, readers: Iterable[FileBasedReader])
{
  private val companionToReader: Map[TypedPath.AnyCompanion, FileBasedReader] = readers toKeyedMap (_.typedPathCompanion)
  private val typedPathCompanions = readers map (_.companion.typedPathCompanion)

  // For tests
  def readCompleteDirectory(): Checked[Seq[FileBased]] =
    readFileBaseds(DirectoryReader.files(directory))

  def readFileBaseds(files: Seq[Path]): Checked[Seq[FileBased]] = {
    val checkedTypedFiles = files.map(toTypedFile)
    TypedFile.checkUniqueness(checkedTypedFiles collect { case Right(o) => o }) match {
      case Left(problem) =>
        Left(Problem.Combined(checkedTypedFiles.collect { case Left(p) => p } :+ problem))
      case Right(_) =>
        checkedTypedFiles.traverseAndCombineProblems(typedFile => toCheckedFileBased(readTypedSource(typedFile)))
    }
  }

  private def toTypedFile(file: Path): Checked[TypedFile] =
    TypedFile.checked(directory, file, typedPathCompanions)

  private def readTypedSource(typedFile: TypedFile): TypedSource =
    TypedSource(typedFile.file.byteString, typedFile.path, typedFile.sourceType)

  private def toCheckedFileBased(o: TypedSource): Checked[FileBased] =
    companionToReader(o.path.companion).readUntyped(o.path, o.byteString, o.sourceType)
}
