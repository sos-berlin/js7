package js7.core.crypt.x509

import java.security.spec.PKCS8EncodedKeySpec
import java.security.{KeyFactory, PrivateKey, Signature}
import js7.base.auth.Pem
import js7.base.crypt.DocumentSigner
import js7.base.data.ByteArray
import js7.base.generic.SecretString
import js7.base.problem.Checked._
import js7.base.problem.{Checked, Problem}
import js7.common.process.Processes.runProcess
import js7.common.scalautil.FileUtils.syntax._
import js7.common.scalautil.FileUtils.withTemporaryDirectory

final class X509Signer private(
  x509PrivateKey: PrivateKey,
  algorithm: X509Algorithm = X509Signature.defaultAlgorithm)
extends DocumentSigner
{
  protected type MySignature = X509Signature

  def companion = X509Signer

  def sign(message: ByteArray): X509Signature = {
    val signature = Signature.getInstance(algorithm.string);
    signature.initSign(x509PrivateKey)
    signature.update(message.unsafeArray)
    X509Signature(ByteArray.unsafeWrap(signature.sign), algorithm = algorithm)
  }

  override def toString = s"X509Signer($x509PrivateKey)"
}

object X509Signer extends DocumentSigner.Companion
{
  protected type MySignature = X509Signature
  protected type MyMessageSigner = X509Signer

  def typeName = X509Signature.TypeName

  def checked(privateKey: ByteArray, password: SecretString = SecretString("")) =
    if (password.nonEmpty)
      Left(Problem.pure("X.509 private key does not require a password"))
    else
      Checked.catchNonFatal {
        KeyFactory.getInstance("RSA")
          .generatePrivate(new PKCS8EncodedKeySpec(privateKey.toArray))
      }.map(new X509Signer(_))

  private val privateKeyPem = Pem("PRIVATE KEY")

  def forTest(): (X509Signer, X509SignatureVerifier) =
    withTemporaryDirectory("X509Signer") { dir =>
      val privateKeyFile = dir / "private-key"
      val certificateFile = dir / "certificate.pem"
      runProcess(s"openssl req -x509 -newkey rsa:1024 -sha512 -subj '/CN=TEST' -days 2 -nodes " +
        s"-keyout '$privateKeyFile' -out '$certificateFile'")

      val signer = privateKeyPem.fromPem(privateKeyFile.contentString)
        .flatMap(X509Signer.checked(_))
        .orThrow
      val verifier = X509SignatureVerifier.checked(certificateFile.byteArray :: Nil, "forTest")
        .orThrow
      signer -> verifier
    }
}

