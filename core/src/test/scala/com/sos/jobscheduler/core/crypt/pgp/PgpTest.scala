package com.sos.jobscheduler.core.crypt.pgp

import cats.data.Validated.{Invalid, Valid}
import cats.syntax.show._
import com.sos.jobscheduler.base.generic.SecretString
import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.base.utils.SyncResource.ops._
import com.sos.jobscheduler.common.scalautil.GuavaUtils.stringToInputStreamResource
import com.sos.jobscheduler.common.utils.JavaResource
import com.sos.jobscheduler.core.crypt.pgp.PgpCommons.{readPublicKeyRingCollection, toPublicKeyRingCollection, writePublicKeyAsAscii, _}
import com.sos.jobscheduler.core.crypt.pgp.PgpSigner.readSecretKey
import com.sos.jobscheduler.core.crypt.pgp.PgpTest._
import com.sos.jobscheduler.core.problems.{PGPMessageSignedByUnknownProblem, PGPTamperedWithMessageProblem}
import com.sos.jobscheduler.data.crypt.{PgpSignature, SignerId}
import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets.UTF_8
import org.bouncycastle.openpgp.{PGPException, PGPSignature}
import org.scalatest.FreeSpec
import org.scalatest.Matchers._

final class PgpTest extends FreeSpec
{
  private lazy val verifier = new PgpSignatureVerifier(readPublicKeyRingCollection(publicKeyResource))

  "Invalid password for secret key" in {
    for (invalidPassword ← Array("", "INVALID")) {
      val secretKey = readSecretKey(secretKeyResource)
      intercept[PGPException] {
        PgpSigner(secretKey, SecretString(invalidPassword)).orThrow
      }.getMessage shouldEqual "checksum mismatch at 0 of 20"  // TODO Weird Problem message for an invalid password
    }
  }

  "Use predefine private key" - {
    "PGPSecretKey.show" in {
      val signature: PGPSignature = PgpSignatureVerifier.readMutableSignature(stringToInputStreamResource(TestSignature.string)).orThrow
      assert(signature.show == "PGPSignature(F5726E50E5345B98 hash=SHA-256 keyAlgorithm=RSA_GENERAL 2019-01-09T16:32:52Z)")
    }

    "PgpSignatureVerifier" - {
      "toString" in {
        assert(verifier.toString == "PgpSignatureVerifier(userIds='TEST (COMMENT) <test@example.com>' PGPPublicKeyRing(PGPPublicKey(F5726E50E5345B98 2019-01-09T10:24:28Z 'TEST (COMMENT) <test@example.com>' algorithm=RSA_GENERAL isEncryptionKey=true isMasterKey=true fingerprint=D759C7DA556F2EF9CAD52460F5726E50E5345B98), PGPPublicKey(20058E9B9A6C7F27 2019-01-09T10:24:28Z '' algorithm=RSA_GENERAL isEncryptionKey=true isMasterKey=false fingerprint=7105E581003532F143DFDEB720058E9B9A6C7F27)))")
      }

      "Signature by alien key is rejected" in {
        assert(verifier.verify(TestMessage + "X", AlienSignature)
          == Invalid(PGPMessageSignedByUnknownProblem))
      }

      "Changed message is rejected" in {
        val verified = verifier.verify(TestMessage + "X", TestSignature)
        assert(verified == Invalid(PGPTamperedWithMessageProblem))
      }

      "Proper message is accepted" in {
        val verified = verifier.verify(TestMessage, TestSignature)
        assert(verified == Valid(signerIds))
      }
    }

    "PgpSigner" - {
      lazy val signer = PgpSigner(readSecretKey(secretKeyResource), secretKeyPassword).orThrow

      "toString" in {
        assert(signer.toString == "PgpSigner(PGPSecretKey(F5726E50E5345B98 PGPPublicKey(F5726E50E5345B98 2019-01-09T10:24:28Z 'TEST (COMMENT) <test@example.com>' algorithm=RSA_GENERAL isEncryptionKey=true isMasterKey=true fingerprint=D759C7DA556F2EF9CAD52460F5726E50E5345B98) cipher=AES isSigningKey=true isMasterKey=true ))")
      }

      "Sign and verify" in {
        val signature = signer.sign(TestMessage)
        assert(verifier.verify(TestMessage + "X", signature).isInvalid)
        assert(verifier.verify(TestMessage, signature).isValid)
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
      val verifier = new PgpSignatureVerifier(toPublicKeyRingCollection(secretKey.getPublicKey))
      assert(verifier.verify(TestMessage + "X", signature).isInvalid)
      assert(verifier.verify(TestMessage, signature).isValid)
    }

    "writePublicKeyAsAscii" in {
      val publicKeyAscii: Array[Byte] = {
        val out = new ByteArrayOutputStream
        writePublicKeyAsAscii(secretKey.getPublicKey, out)
        out.toByteArray
      }

      val string = new String(publicKeyAscii, UTF_8)
      assert(string startsWith "-----BEGIN PGP PUBLIC KEY BLOCK-----\n")
      assert(string endsWith "-----END PGP PUBLIC KEY BLOCK-----\n")

      val verifier = new PgpSignatureVerifier(readPublicKeyRingCollection(publicKeyAscii.asResource))
      assert(verifier.verify(TestMessage + "X", signature).isInvalid)
      assert(verifier.verify(TestMessage, signature).isValid)
    }
  }
}

object PgpTest
{
  private[crypt] val TestMessage = "The data to be signed\n"
  private[crypt] val signerIds = SignerId("TEST (COMMENT) <test@example.com>") :: Nil

  // Keys and signatur generated gpg (GnuPG/MacGPG2) 2.2.10, libgcrypt 1.8.3
  // gpg --export --armor
  private[crypt] final val publicKeyResource = JavaResource("com/sos/jobscheduler/core/crypt/pgp/test-public-pgp-key.asc")

  private[crypt] val secretKeyPassword = SecretString("TEST-PASSWORD")
  // gpg --export-secret-keys --armore
  private[crypt] final val secretKeyResource = JavaResource("com/sos/jobscheduler/core/crypt/pgp/test-private-pgp-key.asc")

  // gpg --sign --detach-sign 1 && base64 1.sig
  private val TestSignature = PgpSignature(
    JavaResource("com/sos/jobscheduler/core/crypt/pgp/test-pgp-signature.asc").asUTF8String)
  private val AlienSignature = PgpSignature(
    JavaResource("com/sos/jobscheduler/core/crypt/pgp/test-alien-pgp-signature.txt").asUTF8String)
}
