package js7.data.crypt

import io.circe.Decoder
import js7.base.circeutils.CirceUtils._
import js7.base.crypt.{SignatureVerifier, Signed, SignedString, SignerId}
import js7.base.problem.Checked
import js7.data.crypt.VersionedItemVerifier.Verified
import js7.data.item.VersionedItem

/**
  * @author Joacim Zschimmer
  */
final class VersionedItemVerifier[A <: VersionedItem](signatureVerifier: SignatureVerifier, jsonDecoder: Decoder[A])
{
  def verify(signedString: SignedString): Checked[Verified[A]] =
    for {
      signers <- signatureVerifier.verify(signedString)
      json <- signedString.string.parseJson
      item <- jsonDecoder.decodeJson(json).toChecked
    } yield Verified(Signed(item, signedString), signers)
}

object VersionedItemVerifier
{
  final case class Verified[A <: VersionedItem](signedItem: Signed[A], signerIds: Seq[SignerId]) {
    def item: A = signedItem.value

    override def toString =
      s"'${item.id}' verified: signed by ${signerIds.mkString("'", "', '", "'")}"
  }
}
