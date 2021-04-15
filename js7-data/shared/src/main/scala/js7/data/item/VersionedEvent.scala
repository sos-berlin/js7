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
sealed trait VersionedEvent extends NoKeyEvent

object VersionedEvent {
  final case class VersionAdded(versionId: VersionId) extends VersionedEvent

  sealed trait VersionedItemEvent extends VersionedEvent /*with InventoryItemEvent*/ {
    def path: ItemPath
  }

  sealed trait VersionedItemAddedOrChanged extends VersionedItemEvent with Product {
    def signedString = signed.signedString
    def signed: Signed[VersionedItem]
    def path: ItemPath = signed.value.path
    def toShortString = s"$productPrefix($path)"
  }
  object VersionedItemAddedOrChanged
  {
    private[VersionedEvent] def jsonEncoder: Encoder.AsObject[VersionedItemAddedOrChanged] = o =>
      JsonObject(
        "path" -> o.signed.value.path.toTypedString.asJson,
        "signed" -> o.signed.signedString.asJson)

    // TODO Similar to AttachSignedItem
    private[VersionedEvent] def jsonDecoder(implicit x: Decoder[VersionedItem], y: Decoder[ItemPath])
    : Decoder[(VersionedItem, SignedString)] =
      c => for {
        path <- c.get[ItemPath]("path")
        signedString <- c.get[SignedString]("signed")
        parsed <- io.circe.parser.parse(signedString.string)
          .left.map(error => DecodingFailure(error.toString, c.history))
        item <- parsed.as[VersionedItem].flatMap(o =>
          if (o.path != path)
            Left(DecodingFailure(s"Path '$path' in event does not equal path in signed string", c.history))
          else
            Right(o))
      } yield (item, signedString)
  }

  final case class VersionedItemAdded(signed: Signed[VersionedItem]) extends VersionedItemAddedOrChanged {
    def id = signed.value.id
  }
  object VersionedItemAdded
  {
    private[VersionedEvent] implicit val jsonEncoder: Encoder.AsObject[VersionedItemAdded] =
      o => VersionedItemAddedOrChanged.jsonEncoder.encodeObject(o)

    private[VersionedEvent] implicit def jsonDecoder(implicit x: Decoder[VersionedItem], y: Decoder[ItemPath])
    : Decoder[VersionedItemAdded] =
      hCursor =>
        VersionedItemAddedOrChanged.jsonDecoder.decodeJson(hCursor.value).map { case (item, signedString) =>
          new VersionedItemAdded(Signed(item, signedString))
        }
  }

  final case class VersionedItemChanged(signed: Signed[VersionedItem]) extends VersionedItemAddedOrChanged {
    def id = signed.value.id
  }
  object VersionedItemChanged
  {
    private[VersionedEvent] implicit val jsonEncoder: Encoder.AsObject[VersionedItemChanged] =
      o => VersionedItemAddedOrChanged.jsonEncoder.encodeObject(o)

    private[VersionedEvent] implicit def jsonDecoder(implicit x: Decoder[VersionedItem], y: Decoder[ItemPath])
    : Decoder[VersionedItemChanged] =
      hCursor =>
        VersionedItemAddedOrChanged.jsonDecoder.decodeJson(hCursor.value).map { case (item, signedString) =>
          new VersionedItemChanged(Signed(item, signedString))
        }
  }

  final case class VersionedItemDeleted(path: ItemPath) extends VersionedItemEvent {
    require(!path.isAnonymous, "FileChangedChanged event requires a path")
  }

  implicit def jsonCodec(implicit w: Encoder.AsObject[VersionedItem], x: Decoder[VersionedItem],
    y: Encoder[ItemPath], z: Decoder[ItemPath])
  : TypedJsonCodec[VersionedEvent] = TypedJsonCodec(
      Subtype(deriveCodec[VersionAdded]),
      Subtype[VersionedItemAdded],
      Subtype[VersionedItemChanged],
      Subtype(deriveCodec[VersionedItemDeleted]))
}
