package js7.data.item

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, DecodingFailure, JsonObject}
import js7.base.crypt.{Signed, SignedString}
import js7.data.event.JournaledState

trait SignableItem extends InventoryItem
{
  protected type Self <: SignableItem

  val companion: SignableItem.Companion[Self]

  def id: SignableItemId
}

object SignableItem
{
  type Companion_ = Companion[_ <: SignableItem]

  trait Companion[A <: SignableItem] extends InventoryItem.Companion[A]
  {
    type Id <: SignableItemId
    val Id: SignableItemId.Companion[Id]
  }

  // Common JSON serializationfor AgentCommand.AttachSignableItem and SignedItemAdded

  def signedEncodeJson(signedString: SignedString, itemRevision: Option[ItemRevision]) =
    JsonObject(
      "signed" -> signedString.asJson,
      // Store itemRevision separately because it is not included in the SignedString
      "itemRevision" -> itemRevision.asJson)

  def signedJsonDecoder[S <: JournaledState[S]](implicit S: JournaledState.Companion[S])
  : Decoder[Signed[SignableItem]] = {
      import S.signableItemJsonCodec
      c => for {
        signedString <- c.get[SignedString]("signed")
        parsed <- io.circe.parser.parse(signedString.string)
          .left.map(error => DecodingFailure(error.toString, c.history))
        item <- parsed.as[SignableItem]
        itemRevision <- c.get[Option[ItemRevision]]("itemRevision")
      } yield {
        // Add itemRevision because it was not included in the SignedString
        val revItem = (item, itemRevision) match {
          case (item: SignableSimpleItem, Some(rev)) => item.withRevision(Some(rev))
          case _ => item
        }
        Signed(revItem, signedString)
      }
  }
}
