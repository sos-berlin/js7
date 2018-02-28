package com.sos.jobscheduler.core.filebased

import akka.util.ByteString
import cats.instances.vector._
import cats.syntax.flatMap._
import cats.syntax.traverse._
import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.base.utils.Collections.implicits.RichTraversable
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits.RichPath
import com.sos.jobscheduler.data.filebased.{FileBased, SourceType, TypedPath}
import java.nio.file.Path
import scala.collection.immutable.{Iterable, Seq}

/**
  * @author Joacim Zschimmer
  */
trait FileBasedReader
{
  val fileBasedCompanion: FileBased.Companion_

  protected/*IntelliJ*/ type ThisFileBased = fileBasedCompanion.ThisFileBased
  protected/*IntelliJ*/ type ThisTypedPath = fileBasedCompanion.ThisTypedPath

  def read(path: ThisTypedPath, byteString: ByteString): PartialFunction[SourceType, Checked[ThisFileBased]]

  private def readUntyped(path: TypedPath, byteString: ByteString, sourceType: SourceType): Checked[ThisFileBased] = {
    assert(path.companion eq typedPathCompanion, "FileBasedReader readUntyped")
    read(path.asInstanceOf[ThisTypedPath], byteString).applyOrElse(sourceType,
      (_: SourceType) ⇒ Problem(s"Unrecognized SourceType $sourceType for path '$path'"))
  }

  final def typedPathCompanion: TypedPath.Companion[ThisTypedPath] = fileBasedCompanion.typedPathCompanion
}

object FileBasedReader
{
  def readDirectoryTreeFlattenProblems(directory: Path, readers: Iterable[FileBasedReader], ignoreAliens: Boolean = false): Checked[Seq[FileBased]] =
    for {
      checkedFileBasedIterator ← readDirectoryTree(directory, readers, ignoreAliens = ignoreAliens)
      fileBaseds ← checkedFileBasedIterator.toVector.sequence
    } yield fileBaseds

  def readDirectoryTree(directory: Path, readers: Iterable[FileBasedReader], ignoreAliens: Boolean = false): Checked[Iterator[Checked[FileBased]]] = {
    val typedSourceReader = new TypedSourceReader(readers)
    val typedFiles = TypedPathDirectoryWalker.typedFiles(directory, readers.map(_.typedPathCompanion), ignoreAliens = ignoreAliens)
    for (typedFiles ← TypedPathDirectoryWalker.checkUniqueness(typedFiles)) yield
      for (checkedTypedFile ← typedFiles.iterator) yield
        for {
          typedFile ← checkedTypedFile
          fileBased ← typedSourceReader.apply(TypedSource(typedFile.file.byteString, typedFile.path, typedFile.sourceType))
        } yield fileBased
  }

  final class TypedSourceReader(readers: Iterable[FileBasedReader]) {
    val companionToReader: Map[TypedPath.AnyCompanion, FileBasedReader] = readers toKeyedMap (_.typedPathCompanion)

    def apply(o: TypedSource): Checked[FileBased] =
      companionToReader(o.path.companion)
        .readUntyped(o.path, o.byteString, o.sourceType)
        .withProblemKey(s"${o.path} (${o.sourceType})")
  }

  final case class TypedSource(byteString: ByteString, path: TypedPath, sourceType: SourceType)
}
