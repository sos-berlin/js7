package com.sos.jobscheduler.data.filebased

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, JsonObject, ObjectEncoder}

/**
  * @author Joacim Zschimmer
  */
final case class FileBasedId[P <: TypedPath](path: P, versionId: VersionId)
{
  //def string = path.string + VersionSeparator + versionId.string

  def requireNonAnonymous(): this.type = {
    path.requireNonAnonymous()
    versionId.requireNonAnonymous()
    this
  }

  def isAnonymous = path.isAnonymous && versionId.isAnonymous

  def pretty = s"${path.pretty} ${versionId.string}"

  override def toString = s"$path ${versionId.string}"
}

object FileBasedId {
  implicit def ordering[P <: TypedPath]: Ordering[FileBasedId[P]] =
    Ordering.by(o ⇒ (o.path, o.versionId))

  implicit def jsonEncoder[P <: TypedPath: Encoder]: ObjectEncoder[FileBasedId[P]] =
    o ⇒ JsonObject(
      "path" → o.path.asJson,
      "versionId" → o.versionId.asJson)

  implicit def jsonDecoder[P <: TypedPath: Decoder]: Decoder[FileBasedId[P]] =
    cursor ⇒
      for {
        path ← cursor.get[P]("path")
        version ← cursor.get[VersionId]("versionId")
      } yield FileBasedId(path, version)

  trait Companion[P <: TypedPath] {
    //def checked(string: String)(implicit P: TypedPath.Companion[P]): Checked[FileBasedId[P]] =
    //  string indexOf VersionSeparator match {
    //    case -1 ⇒ Problem(s"FileBasedIdPath without version (denoted by '$VersionSeparator')?: $string")
    //    case i ⇒ Valid(new FileBasedId(P(string take i), VersionId(string drop i + 1)))
    //  }
  }
}
