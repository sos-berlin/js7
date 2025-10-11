package js7.base.crypt.silly

import js7.base.Problems.TamperedWithSignedMessageProblem
import js7.base.crypt.{GenericSignature, SignatureVerifier, SignerId}
import js7.base.data.ByteArray
import js7.base.problem.Problem
import js7.base.utils.Assertions.assertThat
import js7.base.utils.Labeled
import org.jetbrains.annotations.TestOnly

/**
  * @author Joacim Zschimmer
  */
final class SillySignatureVerifier(signatures: Seq[SillySignature], val publicKeyOrigin: String)
extends SignatureVerifier
{
  import SillySignatureVerifier.*

  def this() = this(SillySignature.Default :: Nil, publicKeyOrigin = "Silly")

  def allowExpiredCert = false

  protected type MySignature = SillySignature

  def companion = SillySignatureVerifier

  @TestOnly
  def publicKeys = signatures.map(_.string)

  def verify(document: ByteArray, signature: SillySignature) =
    if (!signatures.contains(signature))
      Left(TamperedWithSignedMessageProblem)
    else
      Right(SillySignerId :: Nil)

  override def toString = s"SillySignatureVerifer(origin=$publicKeyOrigin)"

  def publicKeysToStrings =
    s"$typeName(origin=$publicKeyOrigin)" :: Nil
}

object SillySignatureVerifier extends SignatureVerifier.Companion
{
  protected type MySignature = SillySignature
  protected type MySignatureVerifier = SillySignatureVerifier

  val Default = new SillySignatureVerifier(SillySignature.Default :: Nil, "SillySignatureVerifier.Default")
  val typeName = SillySignature.TypeName
  val filenameExtension = ".silly"
  val recommendedKeyDirectoryName = "trusted-silly-signature-keys"

  private val SillySignerId = SignerId("Silly")

  def checked(publicKeys: Seq[Labeled[ByteArray]], origin: String = "Silly",
    allowExpiredCert: Boolean = false) =
    Right(ignoreInvalid(publicKeys))

  def ignoreInvalid(publicKeys: Seq[Labeled[ByteArray]], origin: String = "Silly",
    allowExpiredCert: Boolean = false) =
    new SillySignatureVerifier(
      publicKeys.map(o => SillySignature(o.value.utf8String)),
      publicKeyOrigin = origin)

  def genericSignatureToSignature(signature: GenericSignature, allowExpiredCert: Boolean = false) =
  {
    assertThat(signature.typeName == typeName)
    if (signature.signerId.isDefined)
      Left(Problem("Silly signature does not accept a signerId"))
    else if (signature.algorithm.isDefined)
      Left(Problem("Silly signature does not accept a signature algorithm"))
    else if (signature.signerCertificate.isDefined)
      Left(Problem("Silly signature does not accept a signature public key"))
    else
      Right(SillySignature(signature.signatureString))
  }
}
