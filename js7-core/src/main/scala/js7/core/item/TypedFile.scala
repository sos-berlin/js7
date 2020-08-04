package js7.core.item

import java.nio.file.Path
import js7.base.problem.{Checked, Problem}
import js7.core.item.TypedPaths.fileToTypedPathAndSourceType
import js7.data.item.{SourceType, TypedPath}

/**
  * @author Joacim Zschimmer
  */
final case class TypedFile(file: Path, path: TypedPath, sourceType: SourceType)

object TypedFile
{
  def checked(baseDirectory: Path, path: Path, companions: Iterable[TypedPath.AnyCompanion]): Checked[TypedFile] =
    fileToTypedPathAndSourceType(companions, baseDirectory, path)
      .map { case (typedPath, sourceType) => TypedFile(path, typedPath, sourceType) }

  def checkUniqueness(typedFiles: Seq[TypedFile]): Checked[typedFiles.type] = {
    val duplicateFiles: Iterable[Path] =
      typedFiles groupBy (_.path) filter (_._2.lengthIs >= 2) flatMap (_._2 map (_.file))
    if (duplicateFiles.isEmpty)
      Right(typedFiles)
    else
      Left(Problem(s"Duplicate configuration files: ${duplicateFiles.toVector.sorted mkString ", "}"))
  }
}
