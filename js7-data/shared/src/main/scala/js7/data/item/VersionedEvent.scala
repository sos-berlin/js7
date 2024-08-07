package js7.data.item

import io.circe.generic.semiauto.deriveCodec
import io.circe.{Decoder, DecodingFailure, Encoder}
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.crypt.{Signed, SignedString}
import js7.base.utils.IntelliJUtils.intelliJuseImport
import js7.base.utils.ScalaUtils.implicitClass
import js7.base.utils.ScalaUtils.syntax.RichJavaClass
import js7.data.controller.ControllerState
import js7.data.controller.ControllerState.{versionedItemJsonCodec, versionedItemPathJsonCodec}
import js7.data.event.NoKeyEvent
import js7.data.item.SignedItemEvent.{SignedItemAdded, SignedItemChanged}
import scala.reflect.ClassTag

/**
  * @author Joacim Zschimmer
  */
sealed trait VersionedEvent extends NoKeyEvent


object VersionedEvent:
  final case class VersionAdded(versionId: VersionId) extends VersionedEvent

  sealed trait VersionedItemEvent extends VersionedEvent:
    def path: VersionedItemPath

  sealed trait VersionedItemAddedOrChanged
  extends VersionedItemEvent,
    // SignedItemAddedOrChanged, ???
    ItemAddedOrChanged,
    Product:

    def signedString: SignedString = signed.signedString
    def signed: Signed[VersionedItem]
    def item: VersionedItem = signed.value
    def path: VersionedItemPath = item.path
    override def toShortString = s"$productPrefix($path)"
  object VersionedItemAddedOrChanged:
    def unapply(event: VersionedItemAddedOrChanged): Some[Signed[VersionedItem]] =
      Some(event.signed)

    // Use SignedItemAdded implementation (for ..AddedOrChanged and ..Changed events)
    private[VersionedEvent] def jsonDecoder[A <: VersionedItemAddedOrChanged: ClassTag]
      (toA: Signed[VersionedItem] => A)
    : Decoder[A] =
      c => SignedItemAdded.jsonCodec[ControllerState]
        .decodeJson(c.value)
        .flatMap(e =>
          e.signed.value match {
            case item: VersionedItem =>
              Right(toA(e.signed.copy(value = item)))
            case item =>
              Left(DecodingFailure(
                s"VersionedItem expected in ${implicitClass.simpleName} event: ${item.key}",
                c.history))
          })

  final case class VersionedItemAdded(signed: Signed[VersionedItem])
  extends VersionedItemAddedOrChanged:
    def id = signed.value.key
  object VersionedItemAdded:
    private[VersionedEvent] implicit val jsonEncoder: Encoder.AsObject[VersionedItemAdded] =
      SignedItemAdded.jsonCodec[ControllerState]
        .contramapObject(e => SignedItemAdded(e.signed))

    private[VersionedEvent] implicit val jsonDecoder: Decoder[VersionedItemAdded] =
      VersionedItemAddedOrChanged.jsonDecoder(VersionedItemAdded(_))

  final case class VersionedItemChanged(signed: Signed[VersionedItem])
  extends VersionedItemAddedOrChanged:
    def id = signed.value.key
  object VersionedItemChanged:
    private[VersionedEvent] implicit val jsonEncoder: Encoder.AsObject[VersionedItemChanged] =
      SignedItemChanged.jsonCodec[ControllerState].contramapObject(e => SignedItemChanged(e.signed))

    private[VersionedEvent] implicit val jsonDecoder: Decoder[VersionedItemChanged] =
      VersionedItemAddedOrChanged.jsonDecoder(VersionedItemChanged(_))

  final case class VersionedItemRemoved(path: VersionedItemPath) extends VersionedItemEvent:
    require(!path.isAnonymous, "VersionedItemRemoved event requires a path")

  implicit val jsonCodec: TypedJsonCodec[VersionedEvent] = TypedJsonCodec(
    Subtype(deriveCodec[VersionAdded]),
    Subtype[VersionedItemAdded],
    Subtype[VersionedItemChanged],
    Subtype(deriveCodec[VersionedItemRemoved]))

  intelliJuseImport((versionedItemJsonCodec, versionedItemPathJsonCodec))
