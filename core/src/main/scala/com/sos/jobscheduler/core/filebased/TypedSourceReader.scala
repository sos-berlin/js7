package com.sos.jobscheduler.core.filebased

import cats.data.Validated.{Invalid, Valid}
import cats.instances.vector._
import cats.syntax.traverse._
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.base.utils.Collections.implicits.RichTraversable
import com.sos.jobscheduler.common.files.DirectoryReader
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits.RichPath
import com.sos.jobscheduler.data.filebased.{FileBased, TypedPath}
import java.nio.file.Path
import scala.collection.immutable.{Iterable, Seq}

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
    TypedFile.checkUniqueness(checkedTypedFiles collect { case Valid(o) => o }) match {
      case Invalid(problem) =>
        Invalid(Problem.Multiple(checkedTypedFiles.collect { case Invalid(p) => p } :+ problem))
      case Valid(_) =>
        checkedTypedFiles.toVector.traverse(_
          .map(readTypedSource)
          .flatMap(toCheckedFileBased))
    }
  }

  private def toTypedFile(file: Path): Checked[TypedFile] =
    TypedFile.checked(directory, file, typedPathCompanions)

  private def readTypedSource(typedFile: TypedFile): TypedSource =
    TypedSource(typedFile.file.byteString, typedFile.path, typedFile.sourceType)

  private def toCheckedFileBased(o: TypedSource): Checked[FileBased] =
    companionToReader(o.path.companion).readUntyped(o.path, o.byteString, o.sourceType)
}
