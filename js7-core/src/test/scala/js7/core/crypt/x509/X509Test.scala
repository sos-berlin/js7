package js7.core.crypt.x509

import java.nio.file.Path
import js7.base.Problems.TamperedWithSignedMessageProblem
import js7.base.crypt.{SignedString, SignerId, X509Signature}
import js7.base.data.ByteArray
import js7.base.problem.Checked._
import js7.common.log.ScribeUtils.coupleScribeWithSlf4j
import js7.common.process.Processes.runProcess
import js7.common.scalautil.FileUtils.syntax._
import js7.common.scalautil.FileUtils.withTemporaryDirectory
import js7.core.crypt.x509.X509Test._
import org.scalatest.freespec.AnyFreeSpec

final class X509Test extends AnyFreeSpec
{
  coupleScribeWithSlf4j()

  "sign progammatically and verify" in {
    val (signer, verifier) = X509Signer.forTest()
    val document = "TEXT EXAMPLE"
    val signature = signer.signString(document)
    val signedIds = verifier.verify(SignedString(document, signature.toGenericSignature)).orThrow
    assert(signedIds == SignerId("Sun RSA public key, 1024 bits") :: Nil)

    assert(verifier.verify(SignedString(document + "X", signature.toGenericSignature)) ==
      Left(TamperedWithSignedMessageProblem))
  }

  "sign with openssl and verify" in {
    withTemporaryDirectory("X509Test-") { dir =>
      val privateKeyFile = dir / "test.key"
      val documentFile = dir / "document"
      val signatureFile = dir / "document.signature"
      val certificateFile = dir / "test.crt"
      val publicKeyFile = dir / "test.pem"

      val document = "TEXT EXAMPLE"
      dir / "document" := document

      if (true) {
        // https://www.zimuel.it/blog/sign-and-verify-a-file-using-openssl
        // https://pagefault.blog/2019/04/22/how-to-sign-and-verify-using-openssl/
        runProcess(s"openssl genrsa -out '$privateKeyFile' 1024")
        assertPemFile(privateKeyFile, "RSA PRIVATE KEY")

        runProcess(s"openssl rsa -in '$privateKeyFile' -pubout -outform pem -out '$publicKeyFile'")
        assertPemFile(publicKeyFile, "PUBLIC KEY")

        runProcess(s"openssl dgst -sign '$privateKeyFile' -sha512 -out '$signatureFile' '$documentFile'")
        runProcess(s"openssl dgst -verify '$publicKeyFile' -sha512 -signature '$signatureFile' '$documentFile'")
      } else {
        runProcess("openssl req" +
          " -x509" +
          " -sha256" +
          " -newkey rsa:1024" +
          " -days 2" +
          " -subj '/CN=TEST'" +
          " -nodes" +
          s" -keyout '$privateKeyFile'" +
          s" -out '$certificateFile'")
        assertPemFile(certificateFile, "CERTIFICATE")

        runProcess(s"openssl dgst -sha256 -sign '$privateKeyFile' -out '$signatureFile' '$documentFile'")
        runProcess(s"openssl x509 -pubkey -in '$certificateFile' -out '$publicKeyFile'")
        runProcess(s"openssl dgst -sha256 -prverify '$privateKeyFile' -signature '$signatureFile' '$documentFile'")
        assertPemFile(publicKeyFile, "CERTIFICATE")
        runProcess(s"openssl dgst -sha256 -verify '$publicKeyFile' -signature '$signatureFile' '$documentFile'")
        runProcess(s"openssl dgst -sha256 -verify '$certificateFile' -signature '$signatureFile' '$documentFile'")
      }

      val verifier = X509SignatureVerifier.checked(
        ByteArray(publicKeyFile.contentString) :: Nil,
        origin = certificateFile.toString
      ).orThrow
      assert(verifier.verifyString(documentFile.contentString, X509Signature(signatureFile.byteArray)) ==
        Right(SignerId("Sun RSA public key, 1024 bits") :: Nil))
    }
  }
}

object X509Test
{
  private def assertPemFile(file: Path, typ: String): Unit =
    Pem(typ).fromPem(file.contentString).orThrow
}
