package com.sos.jobscheduler.core.crypt.silly

import cats.effect.{Resource, SyncIO}
import com.google.common.io.ByteStreams.toByteArray
import com.sos.jobscheduler.base.utils.SyncResource.syntax.RichResource
import com.sos.jobscheduler.core.crypt.SignatureVerifier
import com.sos.jobscheduler.core.problems.TamperedWithSignedMessageProblem
import com.sos.jobscheduler.data.crypt.{GenericSignature, SignerId}
import java.io.InputStream
import java.nio.charset.StandardCharsets.UTF_8
import scala.collection.immutable.Seq

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
}

object SillySignatureVerifier extends SignatureVerifier.Companion
{
  protected type MySignature = SillySignature
  protected type MySignatureVerifier = SillySignatureVerifier

  val typeName = SillySignature.TypeName
  val recommendedKeyDirectoryName = "trusted-silly-signature-keys"

  private val SillySignerId = SignerId("Silly")

  def checked(publicKeys: Seq[Resource[SyncIO, InputStream]], keyOrigin: String = "Silly") =
    Right(
      new SillySignatureVerifier(
        publicKeys.map(o => SillySignature(new String(o.useSync(toByteArray), UTF_8))),
        keyOrigin = keyOrigin))

  def genericSignatureToSignature(signature: GenericSignature) = {
    assert(signature.typeName == typeName)
    SillySignature(signature.signatureString)
  }
}
