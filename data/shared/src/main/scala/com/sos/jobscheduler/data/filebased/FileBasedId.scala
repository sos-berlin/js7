package com.sos.jobscheduler.data.filebased

import com.sos.jobscheduler.base.utils.ScalazStyle._
import com.sos.jobscheduler.data.filebased.FileBasedId._
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, JsonObject, ObjectEncoder}
import scala.language.implicitConversions

/**
  * @author Joacim Zschimmer
  */
final case class FileBasedId[+P <: TypedPath](path: P, versionId: VersionId)
{
  def requireNonAnonymous(): this.type = {
    path.requireNonAnonymous()
    versionId.requireNonAnonymous()
    this
  }

  def isAnonymous = path.isAnonymous && versionId.isAnonymous

  def toSimpleString = if (versionId.isAnonymous) path.string else path.string + VersionSeparator + versionId.string

  def pretty = if (versionId.isAnonymous) path.pretty else s"${path.pretty} ${versionId.string}"

  def toShortString = if (versionId.isAnonymous) path.string else s"${path.string} ${versionId.string}"

  override def toString = if (versionId.isAnonymous) path.toString else s"$path ${versionId.string}"
}

object FileBasedId {
  private val VersionSeparator = "~"  // Can be used in an Akka actor name

  implicit def pathToFileBasedId[P <: TypedPath](path: P): FileBasedId[P] =
    FileBasedId(path, VersionId.Anonymous)

  implicit def ordering[P <: TypedPath]: Ordering[FileBasedId[P]] =
    Ordering.by(o ⇒ (o.path, o.versionId))

  implicit def jsonEncoder[P <: TypedPath: Encoder]: ObjectEncoder[FileBasedId[P]] =
    o ⇒ JsonObject(
      "path" → (!o.path.isAnonymous ? o.path).asJson,
      "versionId" → (!o.versionId.isAnonymous ? o.versionId).asJson)

  implicit def jsonDecoder[P <: TypedPath: TypedPath.Companion: Decoder]: Decoder[FileBasedId[P]] =
    cursor ⇒
      for {
        path ← cursor.get[Option[P]]("path") map (_ getOrElse implicitly[TypedPath.Companion[P]].Anonymous)
        version ← cursor.get[Option[VersionId]]("versionId") map (_ getOrElse VersionId.Anonymous)
      } yield FileBasedId(path, version)

  trait Companion[P <: TypedPath] {
    //def checked(string: String)(implicit P: TypedPath.Companion[P]): Checked[FileBasedId[P]] =
    //  string indexOf VersionSeparator match {
    //    case -1 ⇒ Problem(s"FileBasedIdPath without version (denoted by '$VersionSeparator')?: $string")
    //    case i ⇒ Valid(new FileBasedId(P(string take i), VersionId(string drop i + 1)))
    //  }
  }
}
