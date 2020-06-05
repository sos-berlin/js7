package js7.base.crypt.silly

import cats.effect.{Resource, SyncIO}
import js7.base.Problems.TamperedWithSignedMessageProblem
import js7.base.crypt.{GenericSignature, SignatureVerifier, SignerId}
import js7.base.utils.Assertions.assertThat
import js7.base.utils.SyncResource.syntax.RichResource
import java.io.InputStream
import java.nio.charset.StandardCharsets.UTF_8
import scodec.bits.ByteVector

/**
  * @author Joacim Zschimmer
  */
final class SillySignatureVerifier(signatures: Seq[SillySignature], val keyOrigin: String)
extends SignatureVerifier
{
  import SillySignatureVerifier._

  def this() = this(SillySignature.Default :: Nil, keyOrigin = "Silly")

  protected type MySignature = SillySignature

  def companion = SillySignatureVerifier

  def keys = signatures.map(_.string.getBytes(UTF_8).toVector)

  def verify(message: String, signature: SillySignature) =
    if (!signatures.contains(signature))
      Left(TamperedWithSignedMessageProblem)
    else
      Right(SillySignerId :: Nil)

  override def toString = s"SillySignatureVerifer(origin=$keyOrigin)"

  def trustedKeysToString = s"$typeName(origin=$keyOrigin)"
}

object SillySignatureVerifier extends SignatureVerifier.Companion
{
  protected type MySignature = SillySignature
  protected type MySignatureVerifier = SillySignatureVerifier

  val Default = new SillySignatureVerifier(SillySignature.Default :: Nil, "SillySignatureVerifier.Default")
  val typeName = SillySignature.TypeName
  val recommendedKeyDirectoryName = "trusted-silly-signature-keys"

  private val SillySignerId = SignerId("Silly")

  def checked(publicKeys: Seq[Resource[SyncIO, InputStream]], keyOrigin: String = "Silly") =
    Right(
      new SillySignatureVerifier(
        publicKeys.map(o => SillySignature(new String(o.useSync(inputStreamToByteArray), UTF_8))),
        keyOrigin = keyOrigin))

  def genericSignatureToSignature(signature: GenericSignature) = {
    assertThat(signature.typeName == typeName)
    SillySignature(signature.signatureString)
  }

  private def inputStreamToByteArray(in: InputStream): Array[Byte] = {
    var result = ByteVector.empty
    val bytes = new Array[Byte](4096)
    var eof = false
    while (!eof) {
      val len = in.read(bytes)
      eof = len <= 0
      if (!eof) {
        result ++= ByteVector(bytes, 0, len)
      }
    }
    result.toArray
  }
}
