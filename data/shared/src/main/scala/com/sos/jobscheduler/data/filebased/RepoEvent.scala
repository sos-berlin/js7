package com.sos.jobscheduler.data.filebased

import com.sos.jobscheduler.base.circeutils.CirceUtils.deriveCodec
import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.data.crypt.SignedString
import com.sos.jobscheduler.data.event.NoKeyEvent
import io.circe.syntax._
import io.circe.{Decoder, DecodingFailure, Encoder, JsonObject, ObjectEncoder}

/**
  * @author Joacim Zschimmer
  */
sealed trait RepoEvent extends NoKeyEvent

object RepoEvent {
  final case class VersionAdded(versionId: VersionId) extends RepoEvent

  sealed trait FileBasedEvent extends RepoEvent {
    def path: TypedPath
  }

  sealed trait FileBasedAddedOrChanged extends FileBasedEvent with Product {
    def signed: SignedString
    def toShortString = s"$productPrefix($path)"
  }
  object FileBasedAddedOrChanged {
    def unapply(o: FileBasedAddedOrChanged) = Some((o.path, o.signed))

    private[RepoEvent] def jsonEncoder: ObjectEncoder[FileBasedAddedOrChanged] = o =>
      JsonObject("signed" -> o.signed.asJson)

    private[RepoEvent] def jsonDecoder(implicit x: Decoder[FileBased]): Decoder[(FileBased, SignedString)] = c =>
      for {
        signed <- c.get[SignedString]("signed")
        parsed <- io.circe.parser.parse(signed.string).left.map(error => DecodingFailure(error.toString, Nil))
        fileBased <- parsed.as[FileBased]
      } yield (fileBased, signed)
  }

  final case class FileBasedAdded(path: TypedPath, signed: SignedString) extends FileBasedAddedOrChanged

  final case class FileBasedChanged(path: TypedPath, signed: SignedString) extends FileBasedAddedOrChanged

  final case class FileBasedDeleted(path: TypedPath) extends FileBasedEvent {
    require(!path.isAnonymous, "FileChangedChanged event requires a path")
  }

  implicit def jsonCodec(implicit w: ObjectEncoder[FileBased], x: Decoder[FileBased], y: Encoder[TypedPath], z: Decoder[TypedPath])
  : TypedJsonCodec[RepoEvent] = TypedJsonCodec(
      Subtype(deriveCodec[VersionAdded]),
      Subtype(deriveCodec[FileBasedAdded]),
      Subtype(deriveCodec[FileBasedChanged]),
      Subtype(deriveCodec[FileBasedDeleted]))
}
