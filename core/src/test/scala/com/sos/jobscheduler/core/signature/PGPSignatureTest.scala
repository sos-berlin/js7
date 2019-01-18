package com.sos.jobscheduler.core.signature

import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.base.generic.SecretString
import com.sos.jobscheduler.base.utils.SyncResource.ops._
import com.sos.jobscheduler.common.scalautil.GuavaUtils.stringToInputStreamResource
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.utils.JavaResource
import com.sos.jobscheduler.core.problems.{PGPMessageSignedByUnknownProblem, PGPTamperedWithMessageProblem}
import com.sos.jobscheduler.core.signature.PGPCommons.writePublicKeyAscii
import com.sos.jobscheduler.core.signature.PGPSignatureTest._
import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets.UTF_8
import java.util.Base64
import org.bouncycastle.openpgp.PGPException
import org.scalatest.FreeSpec
import org.scalatest.Matchers._

final class PGPSignatureTest extends FreeSpec
{
  private lazy val verifier = PGPSignatureVerifier(publicKeyResource)

  "Invalid password for secret key" in {
    for (invalidPassword ← Array("", "INVALID")) {
      val signer = PGPSigner(secretKeyResource, SecretString(invalidPassword))
      intercept[PGPException] {
        signer.sign(stringToInputStreamResource(""))
      }.getMessage shouldEqual "checksum mismatch at 0 of 20"  // TODO Weird Problem message for an invalid password
    }
  }

  "Use predefine private key" - {
    "PGPSignatureVerifier" - {
      "toString" in {
        logger.info(verifier.toString)
      }

      "Signature by alien key is rejected" in {
        assert(verifier.verify(stringToInputStreamResource(TestMessage + "X"), AlienSignature.asResource)
          == Invalid(PGPMessageSignedByUnknownProblem))
      }

      "Changed message is rejected" in {
        val verified = verifier.verify(stringToInputStreamResource(TestMessage + "X"), TestSignatureResource)
        assert(verified == Invalid(PGPTamperedWithMessageProblem))
      }

      "Proper message is accepted" in {
        val verified = verifier.verify(stringToInputStreamResource(TestMessage), TestSignatureResource)
        assert(verified == Valid(pgpUserIds))
      }
    }

    "PGPSigner" - {
      lazy val signer = PGPSigner(secretKeyResource, secretKeyPassword)

      "toString" in {
        logger.info(signer.toString)
      }

      "Sign and verify" in {
        val signature = signer.sign(stringToInputStreamResource(TestMessage))
        assert(verifier.verify(stringToInputStreamResource(TestMessage + "X"), signature.asResource).isInvalid)
        assert(verifier.verify(stringToInputStreamResource(TestMessage), signature.asResource).isValid)
      }
    }
  }

  "Define own private key, sign and verfiy" - {
    val password = SecretString("TESTERS PASSWORD")
    lazy val secretKey = PGPKeyGenerator.generateSecretKey(PGPUserId("TESTER"), password, keySize = 1024/*fast*/)
    lazy val signer = PGPSigner(secretKey.getEncoded.asResource, password)
    lazy val signature = signer.sign(stringToInputStreamResource(TestMessage))

    "generate" in {
      secretKey
    }

    "sign" in {
      signature
    }

    "verify" in {
      val verifier = PGPSignatureVerifier(secretKey.getPublicKey.getEncoded.asResource)
      assert(verifier.verify(stringToInputStreamResource(TestMessage + "X"), signature.asResource).isInvalid)
      assert(verifier.verify(stringToInputStreamResource(TestMessage), signature.asResource).isValid)
    }

    "writePublicKeyAscii" in {
      val publicKeyAscii: Array[Byte] = {
        val out = new ByteArrayOutputStream
        writePublicKeyAscii(secretKey.getPublicKey, out)
        out.toByteArray
      }

      val string = new String(publicKeyAscii, UTF_8)
      assert(string startsWith "-----BEGIN PGP PUBLIC KEY BLOCK-----\n")
      assert(string endsWith "-----END PGP PUBLIC KEY BLOCK-----\n")

      val verifier = PGPSignatureVerifier(publicKeyAscii.asResource)
      assert(verifier.verify(stringToInputStreamResource(TestMessage + "X"), signature.asResource).isInvalid)
      assert(verifier.verify(stringToInputStreamResource(TestMessage), signature.asResource).isValid)
    }
  }
}

object PGPSignatureTest
{
  private val logger = Logger(getClass)
  private val TestMessage = "The data to be signed\n"
  private val pgpUserIds = PGPUserId("TEST (COMMENT) <test@example.com>") :: Nil

  // Keys and signatur generated gpg (GnuPG/MacGPG2) 2.2.10, libgcrypt 1.8.3
  // gpg --export --armor
  private final val publicKeyResource = JavaResource("com/sos/jobscheduler/core/signature/test-pgp-public-key.asc")

  private val secretKeyPassword = SecretString("TEST-PASSWORD")
  // gpg --export-secret-keys --armore
  private final val secretKeyResource = JavaResource("com/sos/jobscheduler/core/signature/test-pgp-private-key.asc")

  // gpg --sign --detach-sign 1 && base64 1.sig
  private val TestSignatureResource = JavaResource("com/sos/jobscheduler/core/signature/test-pgp-signature.asc")
  private val AlienSignature = Base64.getMimeDecoder.decode(
    JavaResource("com/sos/jobscheduler/core/signature/test-pgp-alien-signature.txt").asUTF8String)
}
