package js7.core.crypt.x509

import cats.instances.vector._
import cats.syntax.traverse._
import java.security.spec.X509EncodedKeySpec
import java.security.{KeyFactory, PublicKey, Signature}
import js7.base.Problems.TamperedWithSignedMessageProblem
import js7.base.crypt.{GenericSignature, SignatureVerifier, SignerId, X509Signature}
import js7.base.data.ByteArray
import js7.base.problem.Checked
import js7.core.crypt.x509.X509SignatureVerifier.PublicKeyPem

final class X509SignatureVerifier(x509PublicKeys: Seq[PublicKey], val publicKeyOrigin: String)
extends SignatureVerifier
{
  protected type MySignature = X509Signature

  def companion = X509SignatureVerifier

  def publicKeys = for (o <- x509PublicKeys) yield
    ByteArray(PublicKeyPem.toPem(ByteArray.unsafeWrap(o.getEncoded)))

  def publicKeysToString =
    s"X.509 origin=$publicKeyOrigin" + x509PublicKeys.mkString(", ")

  def verify(document: ByteArray, signature: X509Signature): Checked[Seq[SignerId]] =
    Checked.catchNonFatal {
      x509PublicKeys
        .find { x509PublicKey =>
          tryVerify(document, signature, x509PublicKey)
        } match {
          case None =>
            Left(TamperedWithSignedMessageProblem)
          case Some(x509PublicKey) =>
            Right(SignerId(x509PublicKey.toString.takeWhile(_ != '\n')) :: Nil)  // How to extract a SignerId from x509PublicKeys ???
        }
    }.flatten

  private def tryVerify(document: ByteArray, signature: X509Signature, publicKey: PublicKey): Boolean = {
    val sig = Signature.getInstance(signature.algorithm.string)
    sig.initVerify(publicKey)
    sig.update(document.unsafeArray)
    sig.verify(signature.byteArray.unsafeArray)
  }
}

object X509SignatureVerifier extends SignatureVerifier.Companion
{
  protected type MySignature = X509Signature
  protected type MySignatureVerifier = X509SignatureVerifier

  val typeName = X509Signature.TypeName
  val filenameExtension = ".pem"
  val recommendedKeyDirectoryName = "trusted-x509-keys"

  private val PublicKeyPem = Pem("PUBLIC KEY")

  def checked(publicKeys: Seq[ByteArray], origin: String): Checked[X509SignatureVerifier] =
    publicKeys.toVector.traverse(publicKey =>
      Checked.catchNonFatal {
        for (publicKeyBytes <- PublicKeyPem.fromPem(publicKey.utf8String)) yield
          KeyFactory.getInstance("RSA")
            .generatePublic(new X509EncodedKeySpec(publicKeyBytes.unsafeArray))
      }.flatten
    ).map(new X509SignatureVerifier(_, origin))

  def genericSignatureToSignature(signature: GenericSignature): Checked[X509Signature] =
    X509Signature.fromGenericSignature(signature)
}
