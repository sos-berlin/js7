package com.sos.jobscheduler.core.filebased

import akka.util.ByteString
import com.sos.jobscheduler.base.circeutils.CirceUtils.RichCirceString
import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.common.http.CirceToYaml.yamlToJson
import com.sos.jobscheduler.core.filebased.FileBasedReader._
import com.sos.jobscheduler.data.filebased.{FileBased, FileBasedId, FileBasedId_, SourceType, TypedPath}
import io.circe.Json

/**
  * @author Joacim Zschimmer
  */
trait FileBasedReader
{
  val companion: FileBased.Companion_

  import companion.{ThisFileBased, Path => ThisTypedPath}

  protected def read(id: FileBasedId[ThisTypedPath], byteString: ByteString): PartialFunction[SourceType, Checked[ThisFileBased]]

  def convertFromJson(json: Json): Checked[ThisFileBased]

  private[filebased] def readUntyped(id: FileBasedId_, byteString: ByteString, sourceType: SourceType): Checked[ThisFileBased] = {
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

  private[filebased] def typedPathCompanion: TypedPath.Companion[ThisTypedPath] = companion.typedPathCompanion
}

object FileBasedReader
{
  final case class SourceProblem private(path: TypedPath, sourceType: SourceType, underlying: Problem)
    extends Problem.Lazy(s"Problem with '$path' ($sourceType)", Some(underlying))
}
