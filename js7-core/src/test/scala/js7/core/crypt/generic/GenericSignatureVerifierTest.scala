package js7.core.crypt.generic

import com.typesafe.config.ConfigFactory
import js7.base.Problems.TamperedWithSignedMessageProblem
import js7.base.problem.Checked.Ops
import js7.common.scalautil.FileUtils.syntax._
import js7.common.scalautil.FileUtils.withTemporaryDirectory
import js7.core.crypt.pgp.PgpSigner.readSecretKey
import js7.core.crypt.pgp.{PgpSigner, PgpTest}
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class GenericSignatureVerifierTest extends AnyFreeSpec
{
  "Directory of public keys (recommended usage)" in {
    withTemporaryDirectory("GenericSignatureVerifierTest-") { directory =>
      val messages = List("MESSAGE-1", "Message-2")
      val signers = List(
        PgpSigner(readSecretKey(PgpTest.secretKeyResource), PgpTest.secretKeyPassword).orThrow,
        PgpSigner(readSecretKey(PgpTest.secretKeyResource2), PgpTest.secretKeyPassword2).orThrow)
      val signatures = for ((message, signer) <- messages zip signers)
        yield signer.sign(message).toGenericSignature
      for (signature <- signatures) assert(signature.typeName == "PGP")

      directory / "test.asc" := PgpTest.publicKeyResource.contentBytes
      directory / "test-2.asc" := PgpTest.publicKeyResource2.contentBytes
      directory / ".ignore" := "NOT A SIGNATURE FILE"

      val verifier = GenericSignatureVerifier(ConfigFactory.parseString(
        s"""js7.configuration.trusted-signature-keys.PGP = "$directory" """)
      ).orThrow
      assert(verifier.verify(messages(0), signatures(0)) == Right(PgpTest.signerIds))
      assert(verifier.verify("TAMPERED", signatures(0)) == Left(TamperedWithSignedMessageProblem))
      assert(verifier.verify(messages(1), signatures(1)) == Right(PgpTest.signerIds2))
    }
  }
}
