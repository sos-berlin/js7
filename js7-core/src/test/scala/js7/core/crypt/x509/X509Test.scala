package js7.core.crypt.x509

import java.nio.file.Files.delete
import java.nio.file.Path
import js7.base.Problems.{MessageSignedByUnknownProblem, TamperedWithSignedMessageProblem}
import js7.base.crypt.{GenericSignature, SignedString, SignerId}
import js7.base.data.ByteArray
import js7.base.monixutils.MonixBase.syntax.RichMonixObservable
import js7.base.problem.Checked
import js7.base.problem.Checked._
import js7.base.time.ScalaTime._
import js7.base.time.Stopwatch
import js7.common.log.ScribeUtils.coupleScribeWithSlf4j
import js7.common.process.Processes.runProcess
import js7.common.scalautil.FileUtils.syntax._
import js7.common.scalautil.FileUtils.withTemporaryDirectory
import js7.common.scalautil.MonixUtils.syntax._
import js7.core.crypt.x509.OpensslContext.assertPemFile
import js7.core.crypt.x509.X509Test._
import monix.execution.Scheduler.Implicits.global
import monix.reactive.Observable
import org.scalatest.Assertions._
import org.scalatest.freespec.AnyFreeSpec
import scala.concurrent.duration.Deadline.now
import scala.util.Random

final class X509Test extends AnyFreeSpec
{
  coupleScribeWithSlf4j()

  "Sign progammatically and verify" in {
    val (signer, verifier) = X509Signer.forTest()
    val document = "TEXT EXAMPLE"
    val signature = signer.signString(document)
    val signedIds = verifier.verify(SignedString(document, signature.toGenericSignature)).orThrow
    assert(signedIds == SignerId("CN=TEST") :: Nil)

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

      runProcess(s"openssl req -x509 -sha512 -newkey rsa:1024 -days 2 -subj '/CN=SIGNER' -nodes" +
        s" -keyout '$privateKeyFile' -out '$certificateFile'")
      assertPemFile("CERTIFICATE", certificateFile)

      runProcess(s"sh -c 'openssl x509 -pubkey -noout -in \'$certificateFile\' >\'$publicKeyFile\''")
      assertPemFile("PUBLIC KEY", publicKeyFile)

      runProcess(s"openssl dgst -sha512 -sign '$privateKeyFile' -out '$signatureFile' '$documentFile'")
      runProcess(s"openssl dgst -sha512 -verify '$publicKeyFile' -signature '$signatureFile' '$documentFile'")

      val verifier = X509SignatureVerifier.checked(Seq(certificateFile.byteArray), origin = certificateFile.toString).orThrow
      val signature = X509Signature(signatureFile.byteArray)
      assert(verifier.verifyString(documentFile.contentString, signature) == Right(SignerId("CN=SIGNER") :: Nil))
      assert(verifier.verifyString(documentFile.contentString + "X", signature) == Left(TamperedWithSignedMessageProblem))
    }
  }

  "Sign with certificate and verify signature and certificate against Root" in {
    withTemporaryDirectory("X509Test-") { dir =>
      val openssl = new OpensslContext(dir)
      val ca = new openssl.Root("Root")
      assert(X509.bytesToCertificate(ca.certificateFile.byteArray).orThrow.isCA)

      val documentFile = dir / "document"
      documentFile := "TEST DOCUMENT"

      val signer = new ca.Signer("SIGNER")
      val signatureFile = signer.sign(documentFile)

      assert(verify(ca.certificateFile, documentFile, signatureFile, Some(signer.certificateFile)) ==
        Right(Seq(SignerId("CN=SIGNER"))))

      val alienSigner = new ca.Signer("ALIEN-SIGNER")
      val alienSignatureFile = alienSigner.sign(documentFile)
      assert(verify(signer.certificateFile, documentFile, alienSignatureFile) == Left(TamperedWithSignedMessageProblem))

      val root2 = new openssl.Root("Root-2")
      assert(verify(root2.certificateFile, documentFile, signatureFile, Some(signer.certificateFile)) ==
        Left(MessageSignedByUnknownProblem))
    }
  }

  "Verification agains root certificate requires the critical CA contraint" in {
    withTemporaryDirectory("X509Test-") { dir =>
      val openssl = new OpensslContext(dir)
      val ca = new openssl.Root("Root", suppressCAContraint = true)
      assert(!X509.bytesToCertificate(ca.certificateFile.byteArray).orThrow.isCA)

      val documentFile = dir / "document"
      documentFile := "TEST DOCUMENT"

      val signer = new ca.Signer("SIGNER")
      val signatureFile = signer.sign(documentFile)

      assert(verify(ca.certificateFile, documentFile, signatureFile, Some(signer.certificateFile)) ==
        Left(MessageSignedByUnknownProblem))
    }
  }

  if (sys.props.contains("test.speed")) {
    "Speed test" in {
      val n = 10_000
      withTemporaryDirectory("X509Test-") { dir =>
        val openssl = new OpensslContext(dir)
        val ca = new openssl.Root("Root")
        val signer = new ca.Signer("SIGNER")

        var t = now
        val signedStrings = Observable.fromIterable(1 to n)
          .mapParallelUnorderedBatch() { i =>
            val documentFile = dir / s"document-$i"
            documentFile := Random.nextString(1024)
            val signatureFile = signer.sign(documentFile)
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
        scribe.info(Stopwatch.itemsPerSecondString(t.elapsed, n, "signs"))

        val verifier = X509SignatureVerifier.checked(Seq(ca.certificateFile.byteArray), origin = ca.certificateFile.toString)
          .orThrow
        for (_ <- 1 to 10) {
          t = now
          Observable.fromIterable(signedStrings)
            .mapParallelUnorderedBatch()(signedString =>
              assert(verifier.verify(signedString) == Right(Seq(SignerId("CN=SIGNER")))))
            .completedL.await(999.s)
          scribe.info(Stopwatch.itemsPerSecondString(t.elapsed, n, "verifys"))
        }
      }
    }
  }
}

object X509Test
{
  def verify(certificateFile: Path, documentFile: Path, signatureFile: Path, signersCertificateFile: Option[Path] = None)
  : Checked[Seq[SignerId]] = {
    lazy val verifier = X509SignatureVerifier.checked(Seq(certificateFile.byteArray), origin = certificateFile.toString).orThrow
    val signature = X509Signature(
      ByteArray.fromMimeBase64(signatureFile.contentString).orThrow/*reverse OpensslContext's base64 encoding*/,
      signersCertificateFile.map(file => X509.pemToCertificate(file.contentString).orThrow))
    val verified = verifier.verifyString(documentFile.contentString, signature)
    if (verified.isRight) {
      assert(verifier.verifyString(documentFile.contentString + "X", signature) == Left(TamperedWithSignedMessageProblem))
    }
    verified
  }
}
