package js7.core.crypt.x509

import java.security.spec.X509EncodedKeySpec
import java.security.{KeyFactory, PrivateKey, Signature}
import js7.base.crypt.{DocumentSigner, X509Algorithm, X509Signature}
import js7.base.data.ByteArray
import js7.base.generic.SecretString
import js7.base.problem.{Checked, Problem}

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
    X509Signature(ByteArray.unsafeWrap(signature.sign), algorithm)
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
          .generatePrivate(new X509EncodedKeySpec(privateKey.toArray))
      }.map(new X509Signer(_))

  def forTest(): (X509Signer, X509SignatureVerifier) = {
    val keyPair = X509KeyGenerator.generateKeyPair(keySize = 1024/*fast for test*/)
    new X509Signer(keyPair.getPrivate) ->
      new X509SignatureVerifier(
        X509SignatureVerifier.PublicKeyOnly(keyPair.getPublic) :: Nil,
        publicKeyOrigin = "forTest")
  }
}

