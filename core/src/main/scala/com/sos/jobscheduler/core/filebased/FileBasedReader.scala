package com.sos.jobscheduler.core.filebased

import akka.util.ByteString
import cats.data.Validated.Valid
import cats.instances.vector._
import cats.syntax.traverse._
import com.sos.jobscheduler.base.circeutils.CirceUtils.RichCirceString
import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.base.utils.Collections.implicits.RichTraversable
import com.sos.jobscheduler.common.http.CirceToYaml.yamlToJson
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits.RichPath
import com.sos.jobscheduler.core.filebased.FileBasedReader._
import com.sos.jobscheduler.core.filebased.TypedPathDirectoryWalker.TypedFile
import com.sos.jobscheduler.data.filebased.{FileBased, FileBasedId, FileBasedId_, SourceType, TypedPath}
import io.circe.Json
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

  def convertFromJson(json: Json): Checked[ThisFileBased]

  private def readUntyped(id: FileBasedId_, byteString: ByteString, sourceType: SourceType): Checked[ThisFileBased] = {
    assert(id.path.companion eq typedPathCompanion, "FileBasedReader readUntyped")
    val result: Checked[ThisFileBased] = read(id.asInstanceOf[FileBasedId[ThisTypedPath]], byteString).applyOrElse(sourceType,
      (_: SourceType) ⇒ Problem(s"Unrecognized SourceType '$sourceType' for path '$id'"))
    result.mapProblem(p ⇒ SourceProblem(id.path, sourceType, p))
  }

  final def readJsonString(source: String): Checked[ThisFileBased] =
    source.parseJsonChecked flatMap convertFromJson

  final def readAnonymousJsonLike(sourceType: SourceType.JsonLike, source: ByteString): Checked[ThisFileBased] =
    sourceType match {
      case SourceType.Json ⇒
        readJsonString(source.utf8String)

      case SourceType.Yaml ⇒
        yamlToJson(source.utf8String) flatMap convertFromJson
    }

  private def typedPathCompanion: TypedPath.Companion[ThisTypedPath] = companion.typedPathCompanion
}

object FileBasedReader
{
  def readDirectoryTree(readers: Iterable[FileBasedReader], directory: Path)
  : Checked[Seq[FileBased]] =
    for {
      checkedFileBasedIterator ← readDirectoryTreeWithProblems(readers, directory)
      fileBaseds ← checkedFileBasedIterator.toVector.sequence
    } yield fileBaseds

  @deprecated("read directory then use readObjects")
  def readDirectoryTreeWithProblems(readers: Iterable[FileBasedReader], directory: Path)
  : Checked[Iterator[Checked[FileBased]]] = {
    val typedSourceReader = new TypedSourceReader(readers)
    val typedFiles = TypedPathDirectoryWalker.typedFiles(directory, readers.map(_.typedPathCompanion))
    for (_ ← TypedPathDirectoryWalker.checkUniqueness(typedFiles collect { case Valid(o) ⇒ o })) yield
      for (checkedTypedFile ← typedFiles.iterator) yield
        for {
          typedFile ← checkedTypedFile
          fileBased ← typedSourceReader.apply(TypedSource(typedFile.file.byteString, typedFile.path, typedFile.sourceType))
        } yield fileBased
  }

  def readObjects(readers: Iterable[FileBasedReader], directory: Path, typedFiles: Seq[TypedFile]): Checked[Seq[FileBased]] = {
    val typedSourceReader = new TypedSourceReader(readers)
    TypedPathDirectoryWalker.checkUniqueness(typedFiles).flatMap(_ ⇒
      typedFiles.toVector.traverse(typedFile ⇒
        typedSourceReader.apply(TypedSource(typedFile.file.byteString, typedFile.path, typedFile.sourceType))))
  }

  private class TypedSourceReader(readers: Iterable[FileBasedReader]) {
    val companionToReader: Map[TypedPath.AnyCompanion, FileBasedReader] = readers toKeyedMap (_.typedPathCompanion)

    def apply(o: TypedSource): Checked[FileBased] =
      companionToReader(o.path.companion)
        .readUntyped(o.path, o.byteString, o.sourceType)
  }

  private case class TypedSource(byteString: ByteString, path: TypedPath, sourceType: SourceType)

  final case class SourceProblem private(path: TypedPath, sourceType: SourceType, underlying: Problem)
    extends Problem.Lazy(s"Problem with '$path' ($sourceType)", Some(underlying))
}
