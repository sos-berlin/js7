package com.sos.jobscheduler.master.repo

import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.base.circeutils.CirceUtils.RichJson
import com.sos.jobscheduler.base.generic.SecretString
import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.core.crypt.pgp.PgpCommons.toPublicKeyRingCollection
import com.sos.jobscheduler.core.crypt.pgp.{PgpKeyGenerator, PgpSignatureVerifier, PgpSigner}
import com.sos.jobscheduler.core.problems.PGPTamperedWithMessageProblem
import com.sos.jobscheduler.data.agent.{Agent, AgentPath}
import com.sos.jobscheduler.data.crypt.SignerId
import com.sos.jobscheduler.data.filebased.{FileBased, SignedRepoObject, VersionId}
import com.sos.jobscheduler.data.workflow.WorkflowPath
import com.sos.jobscheduler.data.workflow.parser.WorkflowParser
import com.sos.jobscheduler.master.data.MasterFileBaseds.jsonCodec
import com.sos.jobscheduler.master.repo.SignedRepoObjectVerifierTest._
import io.circe.syntax.EncoderOps
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class SignedRepoObjectVerifierTest extends FreeSpec
{
  private val signedRepoObjectVerifier = new SignedRepoObjectVerifier[FileBased](signatureVerifier)

  "Verify valid objects" in {
    val originalObjects = Vector[FileBased](workflow, agent)
    val signedRepoObjects = originalObjects map (o ⇒ sign(o.asJson.compactPrint))
    assert(signedRepoObjectVerifier.verifyAndDecodeSeq(signedRepoObjects) == Valid(originalObjects map (_ → signerIds)))
  }

  "Verify falsified" in {
    val originalObjects = Vector[FileBased](workflow, agent)
    def tamper(o: SignedRepoObject) = o.copy(message = o.message + "/TAMPERED")
    val signedRepoObjects = originalObjects map (o ⇒ tamper(sign(o.asJson.compactPrint)))
    assert(signedRepoObjectVerifier.verifyAndDecodeSeq(signedRepoObjects) == Invalid(PGPTamperedWithMessageProblem))
  }
}

object SignedRepoObjectVerifierTest
{
  private val versionId = VersionId("1.0")
  private val workflowScript = """define workflow { execute executable="/SCRIPT.cmd", agent="/AGENT"; }"""
  private val workflow = WorkflowParser.parse(WorkflowPath("/WORKFLOW") % versionId, workflowScript).orThrow
  private val agent = Agent(AgentPath("/AGENT") % versionId, "https://localhost")

  private val signerIds = SignerId("SignedRepoObjectVerifierTest") :: Nil

  private val password = SecretString("TEST-PASSWORD")
  lazy val secretKey = PgpKeyGenerator.generateSecretKey(signerIds.head, password, keySize = 1024/*fast*/)
  private val signatureVerifier = new PgpSignatureVerifier(toPublicKeyRingCollection(secretKey.getPublicKey))

  private val signer = PgpSigner(secretKey, password).orThrow

  private def sign(string: String): SignedRepoObject =
    SignedRepoObject(string, signer.sign(string))
}
