package com.sos.jobscheduler.core.filebased

import akka.util.ByteString
import cats.instances.vector._
import cats.syntax.traverse._
import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.base.utils.Collections.implicits.RichTraversable
import com.sos.jobscheduler.base.utils.ScalaUtils.RichEither
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits.RichPath
import com.sos.jobscheduler.core.filebased.FileBasedReader._
import com.sos.jobscheduler.data.filebased.{FileBased, FileBasedId, FileBasedId_, SourceType, TypedPath, VersionId}
import io.circe.Decoder
import io.circe.parser.{parse ⇒ parseJson}
import java.nio.file.Path
import scala.collection.immutable.{Iterable, Seq}

/**
  * @author Joacim Zschimmer
  */
trait FileBasedReader
{
  val companion: FileBased.Companion_

  import companion.{ThisFileBased, Path ⇒ ThisTypedPath}

  protected def read(id: FileBasedId[ThisTypedPath], byteString: ByteString): PartialFunction[SourceType, Checked[ThisFileBased]]

  private def readUntyped(id: FileBasedId_, byteString: ByteString, sourceType: SourceType): Checked[ThisFileBased] = {
    assert(id.path.companion eq typedPathCompanion, "FileBasedReader readUntyped")
    val result: Checked[ThisFileBased] = read(id.asInstanceOf[FileBasedId[ThisTypedPath]], byteString).applyOrElse(sourceType,
      (_: SourceType) ⇒ Problem(s"Unrecognized SourceType $sourceType for path '$id'"))
    result.mapProblem(p ⇒ SourceProblem(id.path, sourceType, p))
  }

  final def typedPathCompanion: TypedPath.Companion[ThisTypedPath] = companion.typedPathCompanion

  protected final def readAnonymousJson[A <: FileBased { type Self = A }: Decoder](source: ByteString): Checked[A] =
    parseJson(source.utf8String).toSimpleChecked flatMap (_.as[A].toSimpleChecked)
}

object FileBasedReader
{
  def readDirectoryTree(readers: Iterable[FileBasedReader], directory: Path, versionId: VersionId, ignoreAliens: Boolean = false): Checked[Seq[FileBased]] =
    for {
      checkedFileBasedIterator ← readDirectoryTreeWithProblems(readers, directory, versionId, ignoreAliens = ignoreAliens)
      fileBaseds ← checkedFileBasedIterator.toVector.sequence
    } yield fileBaseds

  def readDirectoryTreeWithProblems(readers: Iterable[FileBasedReader], directory: Path, versionId: VersionId, ignoreAliens: Boolean = false): Checked[Iterator[Checked[FileBased]]] = {
    val typedSourceReader = new TypedSourceReader(readers, versionId)
    val typedFiles = TypedPathDirectoryWalker.typedFiles(directory, readers.map(_.typedPathCompanion), ignoreAliens = ignoreAliens)
    for (_ ← TypedPathDirectoryWalker.checkUniqueness(typedFiles)) yield
      for (checkedTypedFile ← typedFiles.iterator) yield
        for {
          typedFile ← checkedTypedFile
          fileBased ← typedSourceReader.apply(TypedSource(typedFile.file.byteString, typedFile.path, typedFile.sourceType))
        } yield fileBased
  }

  final class TypedSourceReader(readers: Iterable[FileBasedReader], versionId: VersionId) {
    val companionToReader: Map[TypedPath.AnyCompanion, FileBasedReader] = readers toKeyedMap (_.typedPathCompanion)

    def apply(o: TypedSource): Checked[FileBased] =
      companionToReader(o.path.companion)
        .readUntyped(o.path % versionId, o.byteString, o.sourceType)
  }

  final case class TypedSource(byteString: ByteString, path: TypedPath, sourceType: SourceType)

  final case class SourceProblem private(path: TypedPath, sourceType: SourceType, underlying: Problem)
    extends Problem.Lazy(s"Problem with '$path' ($sourceType)", Some(underlying))
}
