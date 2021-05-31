package js7.data.item

import io.circe.{Decoder, DecodingFailure, Encoder}
import js7.base.circeutils.CirceUtils.deriveCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.crypt.Signed
import js7.base.utils.IntelliJUtils.intelliJuseImport
import js7.base.utils.ScalaUtils.implicitClass
import js7.base.utils.ScalaUtils.syntax.RichJavaClass
import js7.data.controller.ControllerState
import js7.data.controller.ControllerState.{itemPathJsonCodec, versionedItemJsonCodec}
import js7.data.event.NoKeyEvent
import js7.data.item.SignedItemEvent.{SignedItemAdded, SignedItemChanged}
import scala.reflect.ClassTag

/**
  * @author Joacim Zschimmer
  */
sealed trait VersionedEvent extends NoKeyEvent

object VersionedEvent {
  final case class VersionAdded(versionId: VersionId) extends VersionedEvent

  sealed trait VersionedItemEvent extends VersionedEvent {
    def path: ItemPath
  }

  sealed trait VersionedItemAddedOrChanged extends VersionedItemEvent /*with SignedItemAddedOrChanged???*/ with HasInventoryItem with Product {
    def signedString = signed.signedString
    def signed: Signed[VersionedItem]
    def item: VersionedItem = signed.value
    def path: ItemPath = item.path
    def toShortString = s"$productPrefix($path)"
  }
  object VersionedItemAddedOrChanged
  {
    // Use SignedItemAdded implementation (for ..Added and ..Changed events)
    private[VersionedEvent] def jsonDecoder[A <: VersionedItemAddedOrChanged: ClassTag]
      (toA: Signed[VersionedItem] => A)
    : Decoder[A] =
      c => SignedItemAdded.jsonCodec(ControllerState)
        .decodeJson(c.value)
        .flatMap(e =>
          e.signed.value match {
            case item: VersionedItem =>
              Right(toA(e.signed.copy(value = item)))
            case item =>
              Left(DecodingFailure(
                s"VersionedItem expected in ${implicitClass.simpleName} event: ${item.key}", c.history))
          })
  }

  final case class VersionedItemAdded(signed: Signed[VersionedItem]) extends VersionedItemAddedOrChanged {
    def id = signed.value.key
  }
  object VersionedItemAdded
  {
    private[VersionedEvent] implicit val jsonEncoder: Encoder.AsObject[VersionedItemAdded] =
      SignedItemAdded.jsonCodec(ControllerState)
        .contramapObject(e => SignedItemAdded(e.signed))

    private[VersionedEvent] implicit val jsonDecoder: Decoder[VersionedItemAdded] =
      VersionedItemAddedOrChanged.jsonDecoder(VersionedItemAdded(_))
  }

  final case class VersionedItemChanged(signed: Signed[VersionedItem]) extends VersionedItemAddedOrChanged {
    def id = signed.value.key
  }
  object VersionedItemChanged
  {
    private[VersionedEvent] implicit val jsonEncoder: Encoder.AsObject[VersionedItemChanged] =
      SignedItemChanged.jsonCodec(ControllerState).contramapObject(e => SignedItemChanged(e.signed))

    private[VersionedEvent] implicit val jsonDecoder: Decoder[VersionedItemChanged] =
      VersionedItemAddedOrChanged.jsonDecoder(VersionedItemChanged(_))
  }

  final case class VersionedItemDeleted(path: ItemPath) extends VersionedItemEvent {
    require(!path.isAnonymous, "FileChangedChanged event requires a path")
  }

  implicit val jsonCodec = TypedJsonCodec[VersionedEvent](
    Subtype(deriveCodec[VersionAdded]),
    Subtype[VersionedItemAdded],
    Subtype[VersionedItemChanged],
    Subtype(deriveCodec[VersionedItemDeleted]))

  intelliJuseImport(versionedItemJsonCodec, itemPathJsonCodec)
}
