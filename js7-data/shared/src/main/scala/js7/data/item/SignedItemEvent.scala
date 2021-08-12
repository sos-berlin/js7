package js7.data.item

import io.circe.{Codec, HCursor}
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.crypt.Signed
import js7.data.event.JournaledState

trait SignedItemEvent extends InventoryItemEvent
{
  def key: SignableItemKey
}

object SignedItemEvent
{
  sealed trait SignedItemAddedOrChanged extends SignedItemEvent with ItemAddedOrChanged with Product {
    def signedString = signed.signedString
    def signed: Signed[SignableItem]
    def item: SignableItem = signed.value
    def key: SignableItemKey = signed.value.key
    override def toShortString = s"$productPrefix($key)"
  }

  final case class SignedItemAdded(signed: Signed[SignableItem])
  extends SignedItemAddedOrChanged
  object SignedItemAdded
  {
    def jsonCodec[S <: JournaledState[S]](implicit S: JournaledState.Companion[S])
    : Codec.AsObject[SignedItemAdded]=
      new Codec.AsObject[SignedItemAdded] {
        def encodeObject(o: SignedItemAdded) =
          SignableItem.signedEncodeJson(o.signed.signedString, o.signed.value.itemRevision)

        def apply(c: HCursor) =
         SignableItem.signedJsonDecoder.decodeJson(c.value).map(SignedItemAdded(_))
      }
  }

  final case class SignedItemChanged(signed: Signed[SignableItem])
  extends SignedItemAddedOrChanged
  object SignedItemChanged
  {
    def jsonCodec[S <: JournaledState[S]](implicit S: JournaledState.Companion[S])
    : Codec.AsObject[SignedItemChanged] =
      new Codec.AsObject[SignedItemChanged] {
        def encodeObject(o: SignedItemChanged) =
          SignableItem.signedEncodeJson(o.signedString, o.signed.value.itemRevision)

        def apply(c: HCursor) =
          SignableItem.signedJsonDecoder.decodeJson(c.value).map(SignedItemChanged(_))
      }
  }

  implicit def jsonCodec[S <: JournaledState[S]](implicit S: JournaledState.Companion[S])
  : TypedJsonCodec[SignedItemEvent] =
    TypedJsonCodec(
      Subtype(SignedItemAdded.jsonCodec),
      Subtype(SignedItemChanged.jsonCodec))
}
