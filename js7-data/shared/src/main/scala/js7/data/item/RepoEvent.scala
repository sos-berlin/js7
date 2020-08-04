package js7.data.item

import io.circe.syntax._
import io.circe.{Decoder, DecodingFailure, Encoder, JsonObject}
import js7.base.circeutils.CirceUtils.deriveCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.crypt.{Signed, SignedString}
import js7.data.event.NoKeyEvent

/**
  * @author Joacim Zschimmer
  */
sealed trait RepoEvent extends NoKeyEvent

object RepoEvent {
  final case class VersionAdded(versionId: VersionId) extends RepoEvent

  sealed trait ItemEvent extends RepoEvent {
    def path: TypedPath
  }

  sealed trait ItemAddedOrChanged extends ItemEvent with Product {
    def signedString = signed.signedString
    def signed: Signed[InventoryItem]
    def path: TypedPath = signed.value.path
    def toShortString = s"$productPrefix($path)"
  }
  object ItemAddedOrChanged
  {
    private[RepoEvent] def jsonEncoder: Encoder.AsObject[ItemAddedOrChanged] = o =>
      JsonObject(
        "path" -> o.signed.value.path.toTypedString.asJson,
        "signed" -> o.signed.signedString.asJson)

    private[RepoEvent] def jsonDecoder(implicit x: Decoder[InventoryItem], y: Decoder[TypedPath]): Decoder[(InventoryItem, SignedString)] =
      c => for {
        path <- c.get[TypedPath]("path")
        signed <- c.get[SignedString]("signed")
        parsed <- io.circe.parser.parse(signed.string).left.map(error => DecodingFailure(error.toString, c.history))
        item <- parsed.as[InventoryItem].flatMap(o =>
          if (o.path != path) Left(DecodingFailure(s"Path in event '$path' does not equal path in signed string", c.history))
          else Right(o))
      } yield (item, signed)
  }

  final case class ItemAdded(signed: Signed[InventoryItem]) extends ItemAddedOrChanged
  object ItemAdded
  {
    private[RepoEvent] implicit val jsonEncoder: Encoder.AsObject[ItemAdded] =
      o => ItemAddedOrChanged.jsonEncoder.encodeObject(o)

    private[RepoEvent] implicit def jsonDecoder(implicit x: Decoder[InventoryItem], y: Decoder[TypedPath]): Decoder[ItemAdded] =
      hCursor =>
        ItemAddedOrChanged.jsonDecoder.decodeJson(hCursor.value).map { case (item, signedString) =>
          new ItemAdded(Signed(item, signedString))
        }
  }

  final case class ItemChanged(signed: Signed[InventoryItem]) extends ItemAddedOrChanged
  object ItemChanged
  {
    private[RepoEvent] implicit val jsonEncoder: Encoder.AsObject[ItemChanged] =
      o => ItemAddedOrChanged.jsonEncoder.encodeObject(o)

    private[RepoEvent] implicit def jsonDecoder(implicit x: Decoder[InventoryItem], y: Decoder[TypedPath]): Decoder[ItemChanged] =
      hCursor =>
        ItemAddedOrChanged.jsonDecoder.decodeJson(hCursor.value).map { case (item, signedString) =>
          new ItemChanged(Signed(item, signedString))
        }
  }
  final case class ItemDeleted(path: TypedPath) extends ItemEvent {
    require(!path.isAnonymous, "FileChangedChanged event requires a path")
  }

  implicit def jsonCodec(implicit w: Encoder.AsObject[InventoryItem], x: Decoder[InventoryItem], y: Encoder[TypedPath], z: Decoder[TypedPath])
  : TypedJsonCodec[RepoEvent] = TypedJsonCodec(
      Subtype(deriveCodec[VersionAdded]),
      Subtype[ItemAdded],
      Subtype[ItemChanged],
      Subtype(deriveCodec[ItemDeleted]))
}
