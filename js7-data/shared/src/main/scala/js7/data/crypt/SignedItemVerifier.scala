package js7.data.crypt

import io.circe.Decoder
import js7.base.circeutils.CirceUtils.*
import js7.base.crypt.{SignatureVerifier, Signed, SignedString, SignerId}
import js7.base.problem.Checked
import js7.base.utils.ScalaUtils.implicitClass
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.crypt.SignedItemVerifier.Verified
import js7.data.item.SignableItem
import scala.reflect.ClassTag

/**
  * @author Joacim Zschimmer
  */
final class SignedItemVerifier[A <: SignableItem](signatureVerifier: SignatureVerifier, jsonDecoder: Decoder[A]):
  def verify(signedString: SignedString): Checked[Verified[A]] =
    for
      signers <- signatureVerifier.verify(signedString)
      json <- signedString.string.parseJson
      item <- jsonDecoder.decodeJson(json).toChecked
    yield Verified(Signed(item, signedString), signers)

object SignedItemVerifier:
  final case class Verified[A <: SignableItem](signedItem: Signed[A], signerIds: Seq[SignerId]):
    def ifCast[A1 <: A: ClassTag]: Option[Verified[A1]] =
      implicitClass[A1].isAssignableFrom(signedItem.value.getClass) ?
        this.asInstanceOf[Verified[A1]]

    def item: A = signedItem.value

    override def toString =
      s"'${item.key}' verified: signed by ${signerIds.mkString("'", "', '", "'")}"
