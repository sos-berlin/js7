package js7.common.crypt.generic

import js7.base.Problems.TamperedWithSignedMessageProblem
import js7.base.configutils.Configs.*
import js7.base.crypt.SignedString
import js7.base.data.ByteArray
import js7.base.io.file.FileUtils.syntax.*
import js7.base.io.file.FileUtils.withTemporaryDirectory
import js7.base.problem.Checked.Ops
import js7.base.test.OurTestSuite
import js7.common.crypt.pgp.{PgpSigner, PgpTest}

/**
  * @author Joacim Zschimmer
  */
final class GenericSignatureVerifierTest extends OurTestSuite
{
  "Directory of public keys (recommended usage)" in {
    withTemporaryDirectory("GenericSignatureVerifierTest-") { directory =>
      val messages = List("MESSAGE-1", "Message-2")
      val signers = List(
        PgpSigner.checked(PgpTest.secretKeyResource.readAs[ByteArray], PgpTest.secretKeyPassword).orThrow,
        PgpSigner.checked(PgpTest.secretKeyResource2.readAs[ByteArray], PgpTest.secretKeyPassword2).orThrow)
      val signatures = for ((message, signer) <- messages zip signers)
        yield signer.signString(message).toGenericSignature
      for (signature <- signatures) assert(signature.typeName == "PGP")

      directory / "test.asc" := PgpTest.publicKeyResource.contentBytes
      directory / "test-2.asc" := PgpTest.publicKeyResource2.contentBytes
      directory / ".ignore" := "NOT A SIGNATURE FILE"

      val verifier = GenericSignatureVerifier(config"""
        js7.configuration.trusted-signature-keys.PGP = "${directory.toString.replace("\\", "\\\\")}""""
      ).orThrow
      assert(verifier.verify(SignedString(messages(0), signatures(0))) == Right(PgpTest.signerIds))
      assert(verifier.verify(SignedString("TAMPERED", signatures(0))) == Left(TamperedWithSignedMessageProblem))
      assert(verifier.verify(SignedString(messages(1), signatures(1))) == Right(PgpTest.signerIds2))
    }
  }
}
