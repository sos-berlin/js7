package com.sos.jobscheduler.core.crypt.silly

import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.base.generic.SecretString
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.core.crypt.MessageSigner
import java.nio.charset.StandardCharsets.UTF_8

/**
  * @author Joacim Zschimmer
  */
final class SillySigner(signature: SillySignature) extends MessageSigner
{
  def this() = this(SillySignature.Default)

  protected type MySignature = SillySignature

  def companion = SillySigner

  def sign(message: String) = signature

  def privateKey = signature.string.getBytes(UTF_8).toVector

  def publicKey = privateKey
}

object SillySigner extends MessageSigner.Companion
{
  protected type MySignature = SillySignature
  protected type MyMessageSigner = SillySigner

  def typeName = SillySignature.TypeName

  def checked(privateKey: collection.Seq[Byte], password: SecretString = SecretString("")) =
    if (!password.string.isEmpty )
      Invalid(Problem("Password for SillySigner must be empty"))
    else
      Valid(new SillySigner(SillySignature(new String(privateKey.toArray, UTF_8))))
}
