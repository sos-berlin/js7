package js7.core.filebased

import io.circe.syntax.EncoderOps
import js7.base.Problems.TamperedWithSignedMessageProblem
import js7.base.circeutils.CirceUtils.RichJson
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.crypt.{Signed, SignedString, SignerId}
import js7.base.generic.SecretString
import js7.base.problem.Checked.Ops
import js7.base.utils.CatsUtils.bytesToInputStreamResource
import js7.core.crypt.pgp.PgpCommons.RichPGPPublicKey
import js7.core.crypt.pgp.{PgpKeyGenerator, PgpSignatureVerifier, PgpSigner}
import js7.core.filebased.FileBasedVerifierTest._
import js7.data.crypt.FileBasedVerifier
import js7.data.filebased.{FileBased, FileBasedSigner}
import js7.data.workflow.parser.WorkflowParser
import js7.data.workflow.{Workflow, WorkflowPath}
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class FileBasedVerifierTest extends AnyFreeSpec
{
  "FileBasedSigner.sign" in {
    implicit val jsonCodec = FileBasedVerifierTest.jsonCodec
    val workflowString = jsonCodec(workflow: FileBased).asJson.compactPrint

    def check() = {
      val signature = signer.sign(workflowString).toGenericSignature
      assert(fileBasedSigner.sign(workflow) == SignedString(workflowString, signature))
    }
    try check()
    catch { case _: Throwable =>
      // Generated PGP signature changes every second, so we try again
      check()
    }
  }

  "Verify valid objects" in {
    def check() = {
      val signedString = fileBasedSigner.sign(workflow)
      assert(signedString == fileBasedSigner.sign(workflow))
      assert(fileBasedVerifier.verify(signedString) == Right(FileBasedVerifier.Verified(Signed(workflow, signedString), signerIds)))
    }
    try check()
    catch { case _: Throwable =>
      // Generated PGP signature changes every second, so we try again
      check()
    }
  }

  "Verify falsified" in {
    val tampered = fileBasedSigner.sign(workflow).copy("TAMPERED")
    assert(fileBasedVerifier.verify(tampered) == Left(TamperedWithSignedMessageProblem))
  }
}

object FileBasedVerifierTest
{
  private val workflow = {
    val workflowScript = """define workflow { execute executable="/SCRIPT.cmd", agent="/AGENT"; }"""
    WorkflowParser.parse(WorkflowPath("/WORKFLOW") ~ "1.0", workflowScript).orThrow
  }

  private val signerIds = SignerId("FileBasedVerifierTest") :: Nil

  private val (signer, verifier) = {
    val password = SecretString("TEST-PASSWORD")
    val secretKey = PgpKeyGenerator.generateSecretKey(signerIds.head, password, keySize = 1024/*fast*/)
    val verifier = PgpSignatureVerifier.checked(
      bytesToInputStreamResource(secretKey.getPublicKey.toArmoredAsciiBytes) :: Nil
    ).orThrow
    val signer = PgpSigner(secretKey, password).orThrow
    (signer, verifier)
  }

  private implicit val jsonCodec = TypedJsonCodec[FileBased](
    Subtype(Workflow.jsonEncoder, Workflow.topJsonDecoder))
  private val fileBasedSigner = new FileBasedSigner(signer, jsonCodec)
  private val fileBasedVerifier = new FileBasedVerifier(verifier, jsonCodec)
}
