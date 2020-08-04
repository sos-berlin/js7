package js7.data.crypt

import io.circe.Decoder
import js7.base.circeutils.CirceUtils.{RichCirceString, _}
import js7.base.crypt.{SignatureVerifier, Signed, SignedString, SignerId}
import js7.base.problem.Checked
import js7.data.crypt.InventoryItemVerifier.Verified
import js7.data.item.InventoryItem

/**
  * @author Joacim Zschimmer
  */
final class InventoryItemVerifier[A <: InventoryItem](signatureVerifier: SignatureVerifier, jsonDecoder: Decoder[A])
{
  def verify(signedString: SignedString): Checked[Verified[A]] =
    for {
      signers <- signatureVerifier.verify(signedString)
      json <- signedString.string.parseJsonChecked
      item <- jsonDecoder.decodeJson(json).toChecked
    } yield Verified(Signed(item, signedString), signers)
}

object InventoryItemVerifier
{
  final case class Verified[A <: InventoryItem](signedItem: Signed[A], signerIds: Seq[SignerId]) {
    def item: A = signedItem.value

    override def toString =
      s"'${item.id}' verified: signed by ${signerIds.mkString("'", "', '", "'")}"
  }
}
