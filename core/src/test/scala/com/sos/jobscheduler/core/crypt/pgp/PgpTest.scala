package js7.core.crypt.pgp

import cats.syntax.show._
import js7.base.Problems.{MessageSignedByUnknownProblem, TamperedWithSignedMessageProblem}
import js7.base.crypt.{PgpSignature, SignerId}
import js7.base.generic.SecretString
import js7.base.problem.Checked.Ops
import js7.base.utils.SyncResource.syntax._
import js7.common.scalautil.GuavaUtils.stringToInputStreamResource
import js7.common.utils.JavaResource
import js7.core.crypt.pgp.PgpCommons.{readPublicKeyRingCollection, toPublicKeyRingCollection, writePublicKeyAsAscii, _}
import js7.core.crypt.pgp.PgpSigner.readSecretKey
import js7.core.crypt.pgp.PgpTest._
import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets.UTF_8
import org.bouncycastle.openpgp.{PGPException, PGPSignature}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers._

final class PgpTest extends AnyFreeSpec
{
  private lazy val verifier = new PgpSignatureVerifier(readPublicKeyRingCollection(publicKeyResource.asResource :: Nil), keyOrigin = "PgpTest")

  "Invalid password for secret key" in {
    for (invalidPassword <- Array("", "INVALID")) {
      val secretKey = readSecretKey(secretKeyResource)
      intercept[PGPException] {
        PgpSigner(secretKey, SecretString(invalidPassword)).orThrow
      }.getMessage shouldEqual "checksum mismatch at 0 of 20"  // TODO Weird Problem message for an invalid password
    }
  }

  "Use predefine private key" - {
    "PGPSecretKey.show" in {
      val signature: PGPSignature = PgpSignatureVerifier.readMutableSignature(stringToInputStreamResource(TestSignature.string)).orThrow
      assert(signature.show == "PGPSignature(binary document, created=2019-01-09T16:32:52Z hash=SHA-256 publicKeyID=F5726E50E5345B98)")
    }

    "PgpSignatureVerifier" - {
      "toString" in {
        assert(verifier.toString ==
          "PgpSignatureVerifier(" +
            "origin=PgpTest, " +
            "PGPPublicKeyRing(" +
              "PGPPublicKey(" +
                "F5726E50E5345B98 " +
                "fingerprint=D759 C7DA 556F 2EF9 CAD5 2460 F572 6E50 E534 5B98 " +
                "userIDs='TEST (COMMENT) <test@example.com>' " +
                "created=2019-01-09T10:24:28Z " +
                "algorithm='RSA general' " +
                "isEncryptionKey=true " +
                "isMasterKey=true" +
              "), " +
              "PGPPublicKey(" +
                "20058E9B9A6C7F27 " +
                "fingerprint=7105 E581 0035 32F1 43DF DEB7 2005 8E9B 9A6C 7F27 " +
                "userIDs='' " +
                "created=2019-01-09T10:24:28Z " +
                "algorithm='RSA general' " +
                "isEncryptionKey=true " +
                "isMasterKey=false" +
              ")" +
            ")" +
          ")")
      }

      "Signature by alien key is rejected" in {
        assert(verifier.verify(TestMessage + "X", AlienSignature)
          == Left(MessageSignedByUnknownProblem))
      }

      "Changed message is rejected" in {
        val verified = verifier.verify(TestMessage + "X", TestSignature)
        assert(verified == Left(TamperedWithSignedMessageProblem))
      }

      "Proper message is accepted" in {
        val verified = verifier.verify(TestMessage, TestSignature)
        assert(verified == Right(signerIds))
      }
    }

    "PgpSigner" - {
      lazy val signer = PgpSigner(readSecretKey(secretKeyResource), secretKeyPassword).orThrow

      "toString" in {
        assert(signer.toString ==
          "PgpSigner(" +
            "PGPSecretKey(" +
              "PGPPublicKey(" +
                "F5726E50E5345B98 " +
                "fingerprint=D759 C7DA 556F 2EF9 CAD5 2460 F572 6E50 E534 5B98 " +
                "userIDs='TEST (COMMENT) <test@example.com>' " +
                "created=2019-01-09T10:24:28Z " +
                "algorithm='RSA general' " +
                "isEncryptionKey=true " +
                "isMasterKey=true" +
              ") " +
              "cipher=AES isSigningKey=true isMasterKey=true" +
            ")" +
          ")")
      }

      "Sign and verify" in {
        val signature = signer.sign(TestMessage)
        assert(verifier.verify(TestMessage + "X", signature).isLeft)
        assert(verifier.verify(TestMessage, signature).isRight)
      }
    }
  }

  "Define own private key, sign and verfiy" - {
    val password = SecretString("TESTERS PASSWORD")
    lazy val secretKey = PgpKeyGenerator.generateSecretKey(SignerId("TESTER"), password, keySize = 1024/*fast*/)
    lazy val signer = PgpSigner(secretKey, password).orThrow
    lazy val signature = signer.sign(TestMessage)

    "generate" in {
      secretKey
    }

    "sign" in {
      signature
    }

    "verify" in {
      val verifier = new PgpSignatureVerifier(toPublicKeyRingCollection(secretKey.getPublicKey), "PgpTest")
      assert(verifier.verify(TestMessage + "X", signature).isLeft)
      assert(verifier.verify(TestMessage, signature).isRight)
    }

    "writePublicKeyAsAscii" in {
      val publicKeyAscii: Array[Byte] = {
        val out = new ByteArrayOutputStream
        writePublicKeyAsAscii(secretKey.getPublicKey, out)
        out.toByteArray
      }

      val string = new String(publicKeyAscii, UTF_8)
      assert(string startsWith "-----BEGIN PGP PUBLIC KEY BLOCK-----" + System.lineSeparator)
      assert(string endsWith "-----END PGP PUBLIC KEY BLOCK-----" + System.lineSeparator)

      val verifier = new PgpSignatureVerifier(readPublicKeyRingCollection(publicKeyAscii.asResource :: Nil), "PgpTest")
      assert(verifier.verify(TestMessage + "X", signature).isLeft)
      assert(verifier.verify(TestMessage, signature).isRight)
    }
  }
}

object PgpTest
{
  private[crypt] val TestMessage = "The data to be signed\n"
  private[crypt] val signerIds = SignerId("TEST (COMMENT) <test@example.com>") :: Nil
  private[crypt] val signerIds2 = SignerId("JobScheduler Test (demonstration only) <jobscheduler-test@example.com>") :: Nil

  // Keys and signatur generated gpg (GnuPG/MacGPG2) 2.2.10, libgcrypt 1.8.3
  // gpg --export --armor
  private[crypt] final val publicKeyResource = JavaResource("com/sos/jobscheduler/core/crypt/pgp/public-keys/test-public-pgp-key.asc")
  private[crypt] final val publicKeyResource2 = JavaResource("com/sos/jobscheduler/core/crypt/pgp/public-keys/test-2-public-pgp-key.asc")

  private[crypt] val secretKeyPassword = SecretString("TEST-PASSWORD")
  private[crypt] val secretKeyPassword2 = SecretString("PGP-PASSWORD")
  // gpg --export-secret-keys --armore
  private[crypt] final val secretKeyResource = JavaResource("com/sos/jobscheduler/core/crypt/pgp/test-private-pgp-key.asc")
  private[crypt] final val secretKeyResource2 = JavaResource("com/sos/jobscheduler/core/crypt/pgp/test-2-private-pgp-key.asc")

  // gpg --sign --detach-sign 1 && base64 1.sig
  private val TestSignature = PgpSignature(
    JavaResource("com/sos/jobscheduler/core/crypt/pgp/test-pgp-signature.asc").asUTF8String)
  private val AlienSignature = PgpSignature(
    JavaResource("com/sos/jobscheduler/core/crypt/pgp/test-alien-pgp-signature.txt").asUTF8String)
}
