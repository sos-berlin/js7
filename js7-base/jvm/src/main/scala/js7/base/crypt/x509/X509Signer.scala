package js7.base.crypt.x509

import java.security.spec.PKCS8EncodedKeySpec
import java.security.{KeyFactory, PrivateKey, Signature}
import js7.base.crypt.x509.X509Algorithm.SHA512withRSA
import js7.base.crypt.{DocumentSigner, SignerId}
import js7.base.data.ByteArray
import js7.base.generic.SecretString
import js7.base.io.file.FileUtils.withTemporaryDirectory
import js7.base.problem.Checked.*
import js7.base.problem.{Checked, Problem}

final class X509Signer private(
  x509PrivateKey: PrivateKey,
  val algorithm: X509Algorithm,
  val signerId: SignerId)
extends DocumentSigner
{
  protected type MySignature = X509Signature

  def companion = X509Signer

  def sign(message: ByteArray): X509Signature = {
    val signature = Signature.getInstance(algorithm.string);
    signature.initSign(x509PrivateKey)
    signature.update(message.unsafeArray)
    X509Signature(ByteArray.unsafeWrap(signature.sign), algorithm, Left(signerId))
  }

  override def toString = s"X509Signer($x509PrivateKey)"
}

object X509Signer extends DocumentSigner.Companion
{
  protected type MySignature = X509Signature
  protected type MyMessageSigner = X509Signer

  def typeName = X509Signature.TypeName

  def checked(privateKey: ByteArray, password: SecretString = SecretString("")) =
    if (true)
      Left(Problem.pure("X509Signer requirers a SignerId which the current API does not provide"))
    else if (password.nonEmpty)
      Left(Problem.pure("X.509 private key does not require a password"))
    else
      checked(privateKey, SHA512withRSA, SignerId("???"))

  def checked(privateKey: ByteArray, algorithm: X509Algorithm, signerId: SignerId) =
      Checked.catchNonFatal {
        KeyFactory.getInstance("RSA")
          .generatePrivate(new PKCS8EncodedKeySpec(privateKey.toArray))
      }.map(new X509Signer(_, algorithm, signerId))

  lazy val forTest: (X509Signer, X509SignatureVerifier) =
    newSignerAndVerifier(SignerId("CN=SIGNER"), "forTest")
      .orThrow

  private def newSignerAndVerifier(signerId: SignerId, origin: String)
  : Checked[(X509Signer, X509SignatureVerifier)] =
    withTemporaryDirectory("X509Signer") { dir =>
      val openssl = new Openssl(dir)
      for {
        certWithPrivateKey <- openssl.generateCertWithPrivateKey("X509Signer", s"/${signerId.string}")
        signer <- X509Signer.checked(certWithPrivateKey.privateKey, SHA512withRSA, signerId)
        verifier <- X509SignatureVerifier.checked(certWithPrivateKey.certificate :: Nil, origin)
      } yield signer -> verifier
    }
}
