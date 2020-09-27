package js7.base.crypt.silly

import java.nio.charset.StandardCharsets.UTF_8
import js7.base.crypt.DocumentSigner
import js7.base.data.ByteArray
import js7.base.data.ByteSequence.ops._
import js7.base.generic.SecretString
import js7.base.problem.Problem

/**
  * @author Joacim Zschimmer
  */
final class SillySigner(signature: SillySignature) extends DocumentSigner
{
  protected type MySignature = SillySignature

  def companion = SillySigner

  def sign(document: ByteArray) = signature

  def privateKey = ByteArray(signature.string)

  def toVerifier = new SillySignatureVerifier(signature :: Nil, publicKeyOrigin = "SillySigner")
}

object SillySigner extends DocumentSigner.Companion
{
  protected type MySignature = SillySignature
  protected type MyMessageSigner = SillySigner

  val Default = new SillySigner(SillySignature.Default)

  def typeName = SillySignature.TypeName

  def checked(privateKey: ByteArray, password: SecretString = SecretString.empty) =
    if (!password.string.isEmpty )
      Left(Problem.pure("Password for SillySigner must be empty"))
    else
      Right(new SillySigner(SillySignature(new String(privateKey.toArray, UTF_8))))
}
