package js7.base.crypt.x509

import java.nio.file.Files.delete
import java.nio.file.Path
import js7.base.Problems.{MessageSignedByUnknownProblem, TamperedWithSignedMessageProblem}
import js7.base.crypt.x509.Openssl.{assertPemFile, openssl, quote}
import js7.base.crypt.x509.X509Algorithm.SHA512withRSA
import js7.base.crypt.x509.X509Test.*
import js7.base.crypt.{GenericSignature, SignedString, SignerId}
import js7.base.data.ByteArray
import js7.base.io.file.FileUtils.syntax.*
import js7.base.io.file.FileUtils.withTemporaryDirectory
import js7.base.io.process.Processes.runProcess
import js7.base.log.Logger
import js7.base.monixutils.MonixBase.syntax.RichMonixObservable
import js7.base.problem.Checked.*
import js7.base.problem.{Checked, Problem}
import js7.base.test.OurTestSuite
import js7.base.thread.MonixBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.time.Stopwatch.itemsPerSecondString
import js7.base.utils.Labeled
import monix.execution.Scheduler.Implicits.traced
import monix.reactive.Observable
import org.scalatest.Assertions.*
import scala.concurrent.duration.Deadline.now
import scala.util.Random

final class X509Test extends OurTestSuite
{
  "Sign programmatically and verify" in {
    val (signer, verifier) = X509Signer.forTest
    val document = "TEXT EXAMPLE"
    val signature = signer.signString(document)
    val signerIds = verifier.verify(SignedString(document, signature.toGenericSignature)).orThrow
    assert(signerIds == SignerId("CN=SIGNER") :: Nil)
    assert(signer.signerId == SignerId("CN=SIGNER"))

    assert(verifier.verify(SignedString(document + "X", signature.toGenericSignature)) ==
      Left(TamperedWithSignedMessageProblem))
  }

  "Sign with certificate and verify" in {
    withTemporaryDirectory("X509Test-") { dir =>
      val privateKeyFile = dir / "SIGNER.key"
      val certificateFile = dir / "SIGNER.crt"
      val publicKeyFile = dir / "SIGNER.pem"
      val documentFile = dir / "document"
      val signatureFile = dir / "document.signature"

      val document = "TEXT EXAMPLE"
      dir / "document" := document

      runProcess(s"$openssl req -x509 -newkey rsa:1024 -sha512 -days 2 -nodes -subj '/CN=SIGNER' " +
        s"-keyout ${quote(privateKeyFile)} -out ${quote(certificateFile)} ")
      assertPemFile("CERTIFICATE", certificateFile)

      runProcess(s"""sh -c "openssl x509 -pubkey -noout -in ${quote(certificateFile)} >${quote(publicKeyFile)}"""")
      assertPemFile("PUBLIC KEY", publicKeyFile)

      runProcess(s"$openssl dgst -sha512 -sign ${quote(privateKeyFile)} -out ${quote(signatureFile)} ${quote(documentFile)}")
      runProcess(s"$openssl dgst -sha512 -verify ${quote(publicKeyFile)} -signature ${quote(signatureFile)} ${quote(documentFile)}")

      val certificateBytes = certificateFile.byteArray
      val verifier = X509SignatureVerifier.checked(
        Seq(Labeled(certificateBytes, "X509Test")),
        origin = certificateFile.toString).orThrow
      val signerCert = X509Cert.fromPem(certificateBytes.utf8String).orThrow
      logger.info(signerCert.toLongString)
      val signerId = signerCert.signerId
      assert(!signerId.string.startsWith("/"))
      val signature = X509Signature(signatureFile.byteArray, SHA512withRSA, Left(signerId))
      assert(verifier.verifyString(documentFile.contentString, signature) == Right(SignerId("CN=SIGNER") :: Nil))
      assert(verifier.verifyString(documentFile.contentString + "X", signature) == Left(TamperedWithSignedMessageProblem))
    }
  }

  "Sign with certificate and verify signature and certificate against Root" in {
    withTemporaryDirectory("X509Test-") { dir =>
      val openssl = new Openssl(dir)
      val ca = new openssl.Root("Root")
      val caCert = X509Cert.fromByteArray(ca.certificateFile.byteArray).orThrow
      logger.info(caCert.toLongString)
      assert(caCert.isCA)

      val documentFile = dir / "document"
      documentFile := "TEST DOCUMENT"

      val signer = new ca.Signer("SIGNER")
      val signatureFile = signer.signFile(documentFile)

      assert(verify(ca.certificateFile, documentFile, toSignatureWithTrustedCertificate(signatureFile, signer.certificateFile)) ==
        Right(Seq(SignerId("CN=SIGNER"))))

      val alienSigner = new ca.Signer("ALIEN-SIGNER")
      val alienSignatureFile = alienSigner.signFile(documentFile)
      assert(verify(signer.certificateFile, documentFile, toSignatureWithSignerId(alienSignatureFile, signer.signerId)) ==
        Left(TamperedWithSignedMessageProblem))
      assert(verify(signer.certificateFile, documentFile, toSignatureWithSignerId(alienSignatureFile, alienSigner.signerId)) ==
        Left(Problem("The signature's SignerId is unknown: CN=ALIEN-SIGNER")))

      val root2 = new openssl.Root("Root-2")
      assert(verify(root2.certificateFile, documentFile, toSignatureWithTrustedCertificate(signatureFile, signer.certificateFile)) ==
        Left(TamperedWithSignedMessageProblem))
    }
  }

  "Verification against root certificate requires the critical CA contraint" in {
    pending // openssl 1.1.1i always generates certificates with CA, so this test will fail !!!
    withTemporaryDirectory("X509Test-") { dir =>
      val openssl = new Openssl(dir)
      val ca = new openssl.Root("Root", suppressCAContraint = true)
      // openssl 1.1.1i generates always CA certificates !!!
      // assert(!X509Cert.fromByteArray(ca.certificateFile.byteArray).orThrow.isCA)

      val documentFile = dir / "document"
      documentFile := "TEST DOCUMENT"

      val signer = new ca.Signer("SIGNER")
      val signatureFile = signer.signFile(documentFile)

      val cert = toSignatureWithTrustedCertificate(signatureFile, signer.certificateFile)
      assert(verify(ca.certificateFile, documentFile, cert) == Left(MessageSignedByUnknownProblem))
    }
  }

  if (sys.props.contains("test.speed")) {
    "Speed test" in {
      val n = 10_000
      withTemporaryDirectory("X509Test-") { dir =>
        val openssl = new Openssl(dir)
        val ca = new openssl.Root("Root")
        val signer = new ca.Signer("SIGNER")

        var t = now
        val signedStrings = Observable.fromIterable(1 to n)
          .mapParallelUnorderedBatch() { i =>
            val documentFile = dir / s"document-$i"
            documentFile := Random.nextString(1024)
            val signatureFile = signer.signFile(documentFile)
            val signedString = SignedString(
              documentFile.contentString,
              GenericSignature(
                "X509",
                signatureFile.contentString,
                Some(signer.certificateFile.contentString)))
            delete(documentFile)
            delete(signatureFile)
            signedString
          }
          .toListL.await(999.s)
        logger.info(itemsPerSecondString(t.elapsed, n, "signs"))

        val verifier = X509SignatureVerifier.checked(
            Seq(ca.certificateFile.labeledByteArray),
            origin = ca.certificateFile.toString)
          .orThrow
        for (_ <- 1 to 10) {
          t = now
          Observable.fromIterable(signedStrings)
            .mapParallelUnorderedBatch()(signedString =>
              assert(verifier.verify(signedString) == Right(Seq(SignerId("CN=SIGNER")))))
            .completedL.await(999.s)
          logger.info(itemsPerSecondString(t.elapsed, n, "verifys"))
        }
      }
    }
  }
}

object X509Test
{
  private val logger = Logger[this.type]

  def verify(certificateFile: Path, documentFile: Path, signature: X509Signature): Checked[Seq[SignerId]] = {
    lazy val verifier = X509SignatureVerifier.checked(Seq(certificateFile.labeledByteArray), origin = certificateFile.toString).orThrow
    val verified = verifier.verifyString(documentFile.contentString, signature)
    if (verified.isRight) {
      assert(verifier.verifyString(documentFile.contentString + "X", signature) == Left(TamperedWithSignedMessageProblem))
    }
    verified
  }

  private def toSignatureWithSignerId(signatureFile: Path, signerId: SignerId): X509Signature =
    X509Signature(toSignatureBytes(signatureFile), SHA512withRSA, Left(signerId))

  private def toSignatureWithTrustedCertificate(signatureFile: Path, signersCertificateFile: Path): X509Signature = {
    val cert = X509Cert.fromPem(signersCertificateFile.contentString).orThrow
    logger.info(cert.toLongString)
    X509Signature(toSignatureBytes(signatureFile), SHA512withRSA, Right(cert))
  }

  /** Reverse Openssl's base64 encoding. */
  private def toSignatureBytes(signatureFile: Path) =
    ByteArray.fromMimeBase64(signatureFile.contentString).orThrow
}
