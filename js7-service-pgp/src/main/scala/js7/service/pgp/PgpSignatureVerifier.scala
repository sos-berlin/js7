package js7.service.pgp

import cats.implicits.toBifunctorOps
import cats.syntax.show.*
import java.io.ByteArrayInputStream
import java.nio.charset.StandardCharsets.UTF_8
import js7.base.Problems.{MessageSignedByUnknownProblem, TamperedWithSignedMessageProblem}
import js7.base.crypt.pgp.PgpSignature
import js7.base.crypt.{GenericSignature, SignatureVerifier, SignerId}
import js7.base.data.ByteArray
import js7.base.log.Logger
import js7.base.problem.{Checked, Problem}
import js7.base.utils.Assertions.assertThat
import js7.base.utils.IntelliJUtils.intelliJuseImport
import js7.service.pgp.PgpCommons.*
import org.bouncycastle.openpgp.jcajce.JcaPGPObjectFactory
import org.bouncycastle.openpgp.operator.jcajce.JcaPGPContentVerifierBuilderProvider
import org.bouncycastle.openpgp.{PGPPublicKey, PGPPublicKeyRingCollection, PGPSignature, PGPSignatureList, PGPUtil}
import org.jetbrains.annotations.TestOnly
import scala.jdk.CollectionConverters.*

/**
  * @author Joacim Zschimmer
  */
final class PgpSignatureVerifier(publicKeyRingCollection: PGPPublicKeyRingCollection, val publicKeyOrigin: String)
extends SignatureVerifier
{
  import PgpSignatureVerifier.*

  protected type MySignature = PgpSignature
  def companion = PgpSignatureVerifier

  registerBouncyCastle()

  private val contentVerifierBuilderProvider = new JcaPGPContentVerifierBuilderProvider().setProvider("BC")

  @TestOnly
  def publicKeys = publicKeyRingCollection.toArmoredString :: Nil

  /** Returns `Right(message)` iff signature matches the message. */
  def verify(document: ByteArray, signature: PgpSignature): Checked[Seq[SignerId]] =
    for {
      pgpSignature <- toMutablePGPSignature(signature)
      publicKey <- findPublicKeyInKeyRing(pgpSignature)
      signerIds <- verifyWithPublicKey(document, pgpSignature, publicKey)
    } yield signerIds

  private def findPublicKeyInKeyRing(signature: PGPSignature): Checked[PGPPublicKey] =
    publicKeyRingCollection.getPublicKey(signature.getKeyID) match {
      // Public key is matched with the only 64-bit long key ID ???
      case null =>
        logger.debug(s"$MessageSignedByUnknownProblem, no public key for ${signature.show}")
        Left(MessageSignedByUnknownProblem)
      case publicKey =>
        Right(publicKey)
    }

  private def verifyWithPublicKey(document: ByteArray, pgpSignature: PGPSignature, publicKey: PGPPublicKey)
  : Checked[Seq[SignerId]] = {
    logger.trace("Verifying document with " + pgpSignature.show + ", using " + pgpPublicKeyToShortString(publicKey))
    pgpSignature.init(contentVerifierBuilderProvider, publicKey)
    pgpSignature.update(document.unsafeArray)
    if (!pgpSignature.verify())
      Left(TamperedWithSignedMessageProblem)
    else
      Right(publicKey.getUserIDs.asScala.map(SignerId.apply).toVector)
  }

  override def toString = s"PgpSignatureVerifier(origin=$publicKeyOrigin, ${publicKeyRingCollection.show})"

  def publicKeysToStrings =
    Seq(s"PGP origin=$publicKeyOrigin") ++
      publicKeyRingCollection.asScala
        .flatMap(_.asScala.map(k => "  " + pgpPublicKeyToShortString(k)))
}

object PgpSignatureVerifier extends SignatureVerifier.Companion
{
  protected type MySignature = PgpSignature
  protected type MySignatureVerifier = PgpSignatureVerifier

  val typeName = PgpSignature.TypeName
  val filenameExtension = ".asc"
  val recommendedKeyDirectoryName = "trusted-pgp-keys"

  private val logger = Logger(getClass)

  def checked(publicKeys: Seq[ByteArray], origin: String) =
    Checked.catchNonFatal(
      new PgpSignatureVerifier(readPublicKeyRingCollection(publicKeys), origin)
    ).left.map(o => Problem(s"Error while reading public key '$origin'", cause = Some(o)))

  def genericSignatureToSignature(signature: GenericSignature): Checked[PgpSignature] = {
    assertThat(signature.typeName == typeName)
    if (signature.signerId.isDefined)
      Left(Problem("PGP signature does not accept a signerId"))
    else if (signature.algorithm.isDefined)
      Left(Problem("PGP signature does not accept a signature algorithm"))
    else if (signature.signerCertificate.isDefined)
      Left(Problem("PGP signature does not accept a signed certificate"))
    else {
      val pgpSignature = PgpSignature(signature.signatureString)
      for (_ <- toMutablePGPSignature(pgpSignature)/*check early*/)
        yield pgpSignature
    }
  }

  private[pgp] def toMutablePGPSignature(signature: PgpSignature): Checked[PGPSignature] =
    Checked.catchNonFatal(
      new JcaPGPObjectFactory(PGPUtil.getDecoderStream(new ByteArrayInputStream(signature.string.getBytes(UTF_8))))
        .nextObject
    ) .leftMap(_.withPrefix("Invalid PGP signature: "))
      .flatMap {
        case o: PGPSignatureList =>
          if (o.size != 1)
            Left(Problem(s"Unsupported PGP signature type: exactly one PGPSignature expected, not ${o.size}"))
          else
            Right(o.get(0))

        case null =>
          Left(Problem("Not a valid PGP signature"))

        case o =>
          logger.warn(s"Unsupported PGP signature type: ${o.getClass.getName} $o")
          Left(Problem("Unsupported PGP signature type"))
      }

  intelliJuseImport(PGPPublicKeyShow)
}
