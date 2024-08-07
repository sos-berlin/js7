package js7.base.crypt.silly

import js7.base.Problems.TamperedWithSignedMessageProblem
import js7.base.crypt.{GenericSignature, SignatureVerifier, SignerId}
import js7.base.data.ByteArray
import js7.base.problem.{Checked, Problem}
import js7.base.utils.Assertions.assertThat
import js7.base.utils.Labeled
import org.jetbrains.annotations.TestOnly

/**
  * @author Joacim Zschimmer
  */
final class SillySignatureVerifier(signatures: Seq[SillySignature], val publicKeyOrigin: String)
extends SignatureVerifier:

  import SillySignatureVerifier.*

  def this() = this(SillySignature.Default :: Nil, publicKeyOrigin = "Silly")

  protected type MySignature = SillySignature

  def companion: SillySignatureVerifier.type =
    SillySignatureVerifier

  @TestOnly
  def publicKeys: Seq[String] = signatures.map(_.string)

  def verify(document: ByteArray, signature: SillySignature): Checked[Seq[SignerId]] =
    if !signatures.contains(signature) then
      Left(TamperedWithSignedMessageProblem)
    else
      Right(SillySignerId :: Nil)

  override def toString = s"SillySignatureVerifer(origin=$publicKeyOrigin)"

  def publicKeysToStrings: Seq[String] =
    s"$typeName(origin=$publicKeyOrigin)" :: Nil


object SillySignatureVerifier extends SignatureVerifier.Companion:
  protected type MySignature = SillySignature
  protected type MySignatureVerifier = SillySignatureVerifier

  val Default = new SillySignatureVerifier(SillySignature.Default :: Nil, "SillySignatureVerifier.Default")
  val typeName: String = SillySignature.TypeName
  val filenameExtension = ".silly"
  val recommendedKeyDirectoryName = "trusted-silly-signature-keys"

  private val SillySignerId = SignerId("Silly")

  def checked(publicKeys: Seq[Labeled[ByteArray]], origin: String = "Silly"): Checked[SillySignatureVerifier] =
    Right(ignoreInvalid(publicKeys))

  def ignoreInvalid(publicKeys: Seq[Labeled[ByteArray]], origin: String = "Silly") =
    new SillySignatureVerifier(
      publicKeys.map(o => SillySignature(o.value.utf8String)),
      publicKeyOrigin = origin)

  def genericSignatureToSignature(signature: GenericSignature): Checked[SillySignature] =
    assertThat(signature.typeName == typeName)
    if signature.signerId.isDefined then
      Left(Problem("Silly signature does not accept a signerId"))
    else if signature.algorithm.isDefined then
      Left(Problem("Silly signature does not accept a signature algorithm"))
    else if signature.signerCertificate.isDefined then
      Left(Problem("Silly signature does not accept a signature public key"))
    else
      Right(SillySignature(signature.signatureString))
