package com.sos.jobscheduler.core.crypt.generic

import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.common.scalautil.FileUtils.syntax._
import com.sos.jobscheduler.common.scalautil.FileUtils.withTemporaryFile
import com.sos.jobscheduler.core.crypt.pgp.PgpSigner.readSecretKey
import com.sos.jobscheduler.core.crypt.pgp.{PgpSigner, PgpTest}
import com.sos.jobscheduler.core.problems.TamperedWithSignedMessageProblem
import com.typesafe.config.ConfigFactory
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class GenericSignatureVerifierTest extends FreeSpec
{
  "test" in {
    withTemporaryFile { file =>
      val message = "MESSAGE"
      val signer = PgpSigner(readSecretKey(PgpTest.secretKeyResource), PgpTest.secretKeyPassword).orThrow
      val signature = signer.sign(message).toGenericSignature
      assert(signature.typeName == "PGP")

      file := PgpTest.publicKeyResource.contentBytes

      val verifier = GenericSignatureVerifier(ConfigFactory.parseString(
        s"""jobscheduler.configuration.trusted-signature-keys.PGP = "${file.toString.replace("""\""", """\\""")}"""")
      ).orThrow
      assert(verifier.verify(message, signature) == Right(PgpTest.signerIds))
      assert(verifier.verify("TAMPERED", signature) == Left(TamperedWithSignedMessageProblem))
    }
  }
}
