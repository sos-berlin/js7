package js7.service.pgp

import cats.syntax.show.*
import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets.UTF_8
import js7.base.Problems.{MessageSignedByUnknownProblem, TamperedWithSignedMessageProblem}
import js7.base.crypt.SignerId
import js7.base.crypt.pgp.PgpSignature
import js7.base.data.ByteArray
import js7.base.generic.SecretString
import js7.base.io.JavaResource
import js7.base.problem.Checked.Ops
import js7.base.problem.Problem
import js7.base.test.OurTestSuite
import js7.base.utils.Labeled
import js7.service.pgp.PgpCommons.*
import js7.service.pgp.PgpTest.*
import org.bouncycastle.openpgp.PGPSignature

final class PgpTest extends OurTestSuite:

  private lazy val verifier =
    new PgpSignatureVerifier(
      readPublicKeyRingCollection(Seq(Labeled(publicKeyResource.readAs[ByteArray], publicKeyResource.toString)))
        .orThrow,
      publicKeyOrigin = "PgpTest")

  "Invalid password for secret key" in:
    for invalidPassword <- Array("", "INVALID") do
      assert(PgpSigner.checked(secretKeyResource.readAs[ByteArray], SecretString(invalidPassword)) ==
        Left(Problem(
          // TODO Weird Problem message for an invalid password))
          "org.bouncycastle.openpgp.PGPException: checksum mismatch at in checksum of 20 bytes")))

  "Empty Verfier" in:
    val verifier = PgpSignatureVerifier.checked(Nil).orThrow
    val signer = PgpSigner.checked(secretKeyResource.readAs[ByteArray], secretKeyPassword).orThrow
    assert(verifier.verify(ByteArray.empty, signer.sign(ByteArray.empty)) ==
      Left(MessageSignedByUnknownProblem))

  "Use predefined private key" - {
    "PGPSecretKey.show" in:
      val signature: PGPSignature = PgpSignatureVerifier.toMutablePGPSignature(TestSignature).orThrow
      assert(signature.show == "PGPSignature(binary document, created=2019-01-09T16:32:52Z hash=SHA-256 publicKeyID=F5726E50E5345B98)")

    "PgpSignatureVerifier" - {
      "toString" in:
        assert(verifier.toString ==
          "PgpSignatureVerifier(" +
            "origin=PgpTest, " +
            "PGPPublicKeyRing(" +
              "PGPPublicKey(" +
                "F5726E50E5345B98 " +
                "userIDs='TEST (COMMENT) <test@example.com>' " +
                "fingerprint=d759c7da 556f2ef9 cad52460 f5726e50 e5345b98 " +
                "created=2019-01-09T10:24:28Z " +
                "algorithm='RSA general' " +
                "isEncryptionKey=true " +
                "isMasterKey=true" +
              "), " +
              "PGPPublicKey(" +
                "20058E9B9A6C7F27 " +
                "userIDs='' " +
                "fingerprint=7105e581 003532f1 43dfdeb7 20058e9b 9a6c7f27 " +
                "created=2019-01-09T10:24:28Z " +
                "algorithm='RSA general' " +
                "isEncryptionKey=true " +
                "isMasterKey=false" +
              ")" +
            ")" +
          ")")

      "Signature by alien key is rejected" in:
        assert(verifier.verifyString(TestMessage + "X", AlienSignature)
           == Left(MessageSignedByUnknownProblem))

      "Changed message is rejected" in:
        val verified = verifier.verifyString(TestMessage + "X", TestSignature)
        assert(verified == Left(TamperedWithSignedMessageProblem))

      "Proper message is accepted" in:
        val verified = verifier.verifyString(TestMessage, TestSignature)
        assert(verified == Right(signerIds))
    }

    "PgpSigner" - {
      lazy val signer = PgpSigner.checked(secretKeyResource.readAs[ByteArray], secretKeyPassword).orThrow

      "toString" in:
        assert(signer.toString ==
          "PgpSigner(" +
            "PGPSecretKey(" +
              "PGPPublicKey(" +
                "F5726E50E5345B98 " +
                "userIDs='TEST (COMMENT) <test@example.com>' " +
                "fingerprint=d759c7da 556f2ef9 cad52460 f5726e50 e5345b98 " +
                "created=2019-01-09T10:24:28Z " +
                "algorithm='RSA general' " +
                "isEncryptionKey=true " +
                "isMasterKey=true" +
              ") " +
              "cipher=AES isSigningKey=true isMasterKey=true" +
            ")" +
          ")")

      "Sign and verify" in:
        val signature = signer.signString(TestMessage)
        assert(verifier.verifyString(TestMessage + "X", signature).isLeft)
        assert(verifier.verifyString(TestMessage, signature).isRight)
    }
  }

  "Define own private key, sign and verfiy" - {
    val password = SecretString("TESTERS PASSWORD")
    lazy val secretKey = PgpKeyGenerator.generateSecretKey(SignerId("TESTER"), password, keySize = 1024/*fast*/)
    lazy val signer = PgpSigner(secretKey, password).orThrow
    lazy val signature = signer.signString(TestMessage)

    "generate" in:
      secretKey

    "sign" in:
      signature

    "verify" in:
      val verifier = new PgpSignatureVerifier(toPublicKeyRingCollection(secretKey.getPublicKey), "PgpTest")
      assert(verifier.verifyString(TestMessage + "X", signature).isLeft)
      assert(verifier.verifyString(TestMessage, signature).isRight)

    "writePublicKeyAsAscii" in:
      val publicKeyAscii: Array[Byte] =
        val out = new ByteArrayOutputStream
        writePublicKeyAsAscii(secretKey.getPublicKey, out)
        out.toByteArray

      val string = new String(publicKeyAscii, UTF_8)
      assert(string.startsWith("-----BEGIN PGP PUBLIC KEY BLOCK-----" + System.lineSeparator))
      assert(string.endsWith("-----END PGP PUBLIC KEY BLOCK-----" + System.lineSeparator))

      val verifier = new PgpSignatureVerifier(
        readPublicKeyRingCollection(Seq(Labeled(ByteArray(publicKeyAscii), "PgpTest"))).orThrow,
        "PgpTest")
      assert(verifier.verifyString(TestMessage + "X", signature).isLeft)
      assert(verifier.verifyString(TestMessage, signature).isRight)
  }


object PgpTest:

  private val TestMessage = "The data to be signed\n"
  private[pgp] val signerIds = SignerId("TEST (COMMENT) <test@example.com>") :: Nil
  private[pgp] val signerIds2 = SignerId("JobScheduler Test (demonstration only) <jobscheduler-test@example.com>") :: Nil

  // Keys and signatur generated gpg (GnuPG/MacGPG2) 2.2.10, libgcrypt 1.8.3
  // gpg --export --armor
  private[pgp] final val publicKeyResource = JavaResource("js7/service/pgp/public-keys/test-public-pgp-key.asc")
  private[pgp] final val publicKeyResource2 = JavaResource("js7/service/pgp/public-keys/test-2-public-pgp-key.asc")

  private[pgp] val secretKeyPassword = SecretString("TEST-PASSWORD")
  private[pgp] val secretKeyPassword2 = SecretString("PGP-PASSWORD")
  // gpg --export-secret-keys --armore
  private[pgp] final val secretKeyResource = JavaResource("js7/service/pgp/test-private-pgp-key.asc")
  private[pgp] final val secretKeyResource2 = JavaResource("js7/service/pgp/test-2-private-pgp-key.asc")

  // gpg --sign --detach-sign 1 && base64 1.sig
  private val TestSignature = PgpSignature(
    JavaResource("js7/service/pgp/test-pgp-signature.asc").asUTF8String)
  private val AlienSignature = PgpSignature(
    JavaResource("js7/service/pgp/test-alien-pgp-signature.txt").asUTF8String)
