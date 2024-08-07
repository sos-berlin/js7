package js7.base.crypt.silly

import java.nio.charset.StandardCharsets.UTF_8
import js7.base.crypt.DocumentSigner
import js7.base.data.ByteArray
import js7.base.generic.SecretString
import js7.base.problem.{Checked, Problem}

/**
  * @author Joacim Zschimmer
  */
final class SillySigner(signature: SillySignature) extends DocumentSigner:

  protected type MySignature = SillySignature

  def companion: DocumentSigner.Companion {type MySignature = SillySignature} = SillySigner

  def sign(document: ByteArray): SillySignature = signature

  def toVerifier = new SillySignatureVerifier(signature :: Nil, publicKeyOrigin = "SillySigner")


object SillySigner extends DocumentSigner.Companion:
  protected type MySignature = SillySignature
  protected type MyMessageSigner = SillySigner

  val Default = new SillySigner(SillySignature.Default)

  def typeName: String = SillySignature.TypeName

  def checked(privateKey: ByteArray, password: SecretString = SecretString.empty)
  : Checked[SillySigner] =
    if password.string.nonEmpty then
      Left(Problem.pure("Password for SillySigner must be empty"))
    else
      Right(new SillySigner(SillySignature(new String(privateKey.toArray, UTF_8))))
