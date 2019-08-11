package com.sos.jobscheduler.core.crypt.silly

import com.sos.jobscheduler.core.crypt.SignatureVerifier
import com.sos.jobscheduler.core.problems.TamperedWithSignedMessageProblem
import com.sos.jobscheduler.data.crypt.{GenericSignature, SignerId}
import java.nio.charset.StandardCharsets.UTF_8
import scala.collection.immutable.Seq

/**
  * @author Joacim Zschimmer
  */
final class SillySignatureVerifier(requiredSignature: SillySignature, val keyOrigin: String)
extends SignatureVerifier
{
  import SillySignatureVerifier._

  def this() = this(SillySignature.Default, keyOrigin = "Silly")

  protected type MySignature = SillySignature

  def companion = SillySignatureVerifier

  def key = requiredSignature.string.getBytes(UTF_8).toVector

  def verify(message: String, signature: SillySignature) =
    if (signature != requiredSignature)
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
  val recommendedKeyFileName = "trusted-silly-signature-key.txt"

  private val SillySignerId = SignerId("Silly")

  def checked(publicKey: Seq[Byte], keyOrigin: String = "Silly") =
    Right(
      new SillySignatureVerifier(
        SillySignature(new String(publicKey.toArray, UTF_8)),
        keyOrigin = keyOrigin))

  def genericSignatureToSignature(signature: GenericSignature) = {
    assert(signature.typeName == typeName)
    SillySignature(signature.signatureString)
  }
}
