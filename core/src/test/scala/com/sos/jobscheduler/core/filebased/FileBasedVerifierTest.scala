package com.sos.jobscheduler.core.filebased

import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.base.circeutils.CirceUtils.RichJson
import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.base.generic.SecretString
import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.core.crypt.pgp.PgpCommons.RichPgpPublicKey
import com.sos.jobscheduler.core.crypt.pgp.{PgpKeyGenerator, PgpSignatureVerifier, PgpSigner}
import com.sos.jobscheduler.core.filebased.FileBasedVerifierTest._
import com.sos.jobscheduler.core.problems.TamperedWithSignedMessageProblem
import com.sos.jobscheduler.data.crypt.{Signed, SignedString, SignerId}
import com.sos.jobscheduler.data.filebased.{FileBased, VersionId}
import com.sos.jobscheduler.data.workflow.parser.WorkflowParser
import com.sos.jobscheduler.data.workflow.{Workflow, WorkflowPath}
import io.circe.syntax.EncoderOps
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class FileBasedVerifierTest extends FreeSpec
{
  "FileBasedSigner.sign" in {
    implicit val jsonCodec = FileBasedVerifierTest.jsonCodec
    val workflowString = jsonCodec(workflow: FileBased).asJson.compactPrint
    val signature = signer.sign(workflowString).toGenericSignature
    assert(fileBasedSigner.sign(workflow) == SignedString(workflowString, signature))
  }

  "Verify valid objects" in {
    val signedString = fileBasedSigner.sign(workflow)
    assert(signedString == fileBasedSigner.sign(workflow))
    assert(fileBasedVerifier.verify(signedString) == Valid(Signed(workflow, signedString)))
  }

  "Verify falsified" in {
    val tampered = fileBasedSigner.sign(workflow).copy("TAMPERED")
    assert(fileBasedVerifier.verify(tampered) == Invalid(TamperedWithSignedMessageProblem))
  }
}

object FileBasedVerifierTest
{
  private val workflow = {
    val workflowScript = """define workflow { execute executable="/SCRIPT.cmd", agent="/AGENT"; }"""
    WorkflowParser.parse(WorkflowPath("/WORKFLOW") % VersionId("1.0"), workflowScript).orThrow
  }

  private val (signer, verifier) = {
    val signerIds = SignerId("FileBasedVerifierTest") :: Nil
    val password = SecretString("TEST-PASSWORD")
    val secretKey = PgpKeyGenerator.generateSecretKey(signerIds.head, password, keySize = 1024/*fast*/)
    val verifier = PgpSignatureVerifier.checked(secretKey.getPublicKey.toArmoredAsciiBytes).orThrow
    val signer = PgpSigner(secretKey, password).orThrow
    (signer, verifier)
  }

  private implicit val jsonCodec = TypedJsonCodec[FileBased](
    Subtype(Workflow.jsonEncoder, Workflow.topJsonDecoder))
  private val fileBasedSigner = new FileBasedSigner(signer, jsonCodec)
  private val fileBasedVerifier = new FileBasedVerifier(verifier, jsonCodec)
}
