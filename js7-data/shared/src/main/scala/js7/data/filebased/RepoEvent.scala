package js7.data.filebased

import io.circe.syntax._
import io.circe.{Decoder, DecodingFailure, Encoder, JsonObject}
import js7.base.circeutils.CirceUtils.deriveCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.crypt.{Signed, SignedString}
import js7.base.problem.Problem
import js7.data.event.NoKeyEvent

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
    def signedString = signed.signedString
    def signed: Signed[FileBased]
    def path: TypedPath = signed.value.path
    def toShortString = s"$productPrefix($path)"
  }
  object FileBasedAddedOrChanged
  {
    private[RepoEvent] def jsonEncoder: Encoder.AsObject[FileBasedAddedOrChanged] = o =>
      JsonObject(
        "path" -> o.signed.value.path.toTypedString.asJson,
        "signed" -> o.signed.signedString.asJson)

    private[RepoEvent] def jsonDecoder(implicit x: Decoder[FileBased], y: Decoder[TypedPath]): Decoder[(FileBased, SignedString)] =
      c => for {
        path <- c.get[TypedPath]("path")
        signed <- c.get[SignedString]("signed")
        parsed <- io.circe.parser.parse(signed.string).left.map(error => DecodingFailure(error.toString, c.history))
        fileBased <- parsed.as[FileBased].flatMap(o =>
          if (o.path != path) Left(DecodingFailure(s"Path in event '$path' does not equal path in signed string", c.history))
          else Right(o))
      } yield (fileBased, signed)
  }

  final case class FileBasedAdded(signed: Signed[FileBased]) extends FileBasedAddedOrChanged
  object FileBasedAdded
  {
    private[RepoEvent] implicit val jsonEncoder: Encoder.AsObject[FileBasedAdded] =
      o => FileBasedAddedOrChanged.jsonEncoder.encodeObject(o)

    private[RepoEvent] implicit def jsonDecoder(implicit x: Decoder[FileBased], y: Decoder[TypedPath]): Decoder[FileBasedAdded] =
      hCursor =>
        FileBasedAddedOrChanged.jsonDecoder.decodeJson(hCursor.value).map { case (fileBased, signedString) =>
          new FileBasedAdded(Signed(fileBased, signedString))
        }
  }

  final case class FileBasedChanged(signed: Signed[FileBased]) extends FileBasedAddedOrChanged
  object FileBasedChanged
  {
    private[RepoEvent] implicit val jsonEncoder: Encoder.AsObject[FileBasedChanged] =
      o => FileBasedAddedOrChanged.jsonEncoder.encodeObject(o)

    private[RepoEvent] implicit def jsonDecoder(implicit x: Decoder[FileBased], y: Decoder[TypedPath]): Decoder[FileBasedChanged] =
      hCursor =>
        FileBasedAddedOrChanged.jsonDecoder.decodeJson(hCursor.value).map { case (fileBased, signedString) =>
          new FileBasedChanged(Signed(fileBased, signedString))
        }
  }
  final case class FileBasedDeleted(path: TypedPath) extends FileBasedEvent {
    require(!path.isAnonymous, "FileChangedChanged event requires a path")
  }

  implicit def jsonCodec(implicit w: Encoder.AsObject[FileBased], x: Decoder[FileBased], y: Encoder[TypedPath], z: Decoder[TypedPath])
  : TypedJsonCodec[RepoEvent] = TypedJsonCodec(
      Subtype(deriveCodec[VersionAdded]),
      Subtype[FileBasedAdded],
      Subtype[FileBasedChanged],
      Subtype(deriveCodec[FileBasedDeleted]))
}
