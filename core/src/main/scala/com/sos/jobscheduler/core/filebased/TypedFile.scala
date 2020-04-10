package com.sos.jobscheduler.core.filebased

import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.core.filebased.TypedPaths.fileToTypedPathAndSourceType
import com.sos.jobscheduler.data.filebased.{SourceType, TypedPath}
import java.nio.file.Path

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
      typedFiles groupBy (_.path) filter (_._2.lengthCompare(2) >= 0) flatMap (_._2 map (_.file))
    if (duplicateFiles.isEmpty)
      Right(typedFiles)
    else
      Left(Problem(s"Duplicate configuration files: ${duplicateFiles.toVector.sorted mkString ", "}"))
  }
}
