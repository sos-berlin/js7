package com.sos.jobscheduler.base.crypt.silly

import com.sos.jobscheduler.base.crypt.MessageSigner
import com.sos.jobscheduler.base.generic.SecretString
import com.sos.jobscheduler.base.problem.Problem
import java.nio.charset.StandardCharsets.UTF_8

/**
  * @author Joacim Zschimmer
  */
final class SillySigner(signature: SillySignature) extends MessageSigner
{
  protected type MySignature = SillySignature

  def companion = SillySigner

  def sign(message: String) = signature

  def privateKey = signature.string.getBytes(UTF_8).toVector

  def publicKey = privateKey

  def toVerifier = new SillySignatureVerifier(signature :: Nil, keyOrigin = "SillySigner")

  def verifierCompanion = SillySignatureVerifier
}

object SillySigner extends MessageSigner.Companion
{
  protected type MySignature = SillySignature
  protected type MyMessageSigner = SillySigner

  val Default = new SillySigner(SillySignature.Default)

  def typeName = SillySignature.TypeName

  def checked(privateKey: collection.Seq[Byte], password: SecretString = SecretString("")) =
    if (!password.string.isEmpty )
      Left(Problem("Password for SillySigner must be empty"))
    else
      Right(new SillySigner(SillySignature(new String(privateKey.toArray, UTF_8))))
}
