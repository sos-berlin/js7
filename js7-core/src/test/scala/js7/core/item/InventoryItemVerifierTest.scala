package js7.core.item

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
import js7.core.item.InventoryItemVerifierTest._
import js7.data.crypt.InventoryItemVerifier
import js7.data.item.{InventoryItem, InventoryItemSigner}
import js7.data.workflow.parser.WorkflowParser
import js7.data.workflow.{Workflow, WorkflowPath}
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class InventoryItemVerifierTest extends AnyFreeSpec
{
  "InventoryItemSigner.sign" in {
    implicit val jsonCodec = InventoryItemVerifierTest.jsonCodec
    val workflowString = jsonCodec(workflow: InventoryItem).asJson.compactPrint

    def check() = {
      val signature = signer.sign(workflowString).toGenericSignature
      assert(itemSigner.sign(workflow) == SignedString(workflowString, signature))
    }
    try check()
    catch { case _: Throwable =>
      // Generated PGP signature changes every second, so we try again
      check()
    }
  }

  "Verify valid objects" in {
    def check() = {
      val signedString = itemSigner.sign(workflow)
      assert(signedString == itemSigner.sign(workflow))
      assert(itemVerifier.verify(signedString) == Right(InventoryItemVerifier.Verified(Signed(workflow, signedString), signerIds)))
    }
    try check()
    catch { case _: Throwable =>
      // Generated PGP signature changes every second, so we try again
      check()
    }
  }

  "Verify falsified" in {
    val tampered = itemSigner.sign(workflow).copy("TAMPERED")
    assert(itemVerifier.verify(tampered) == Left(TamperedWithSignedMessageProblem))
  }
}

object InventoryItemVerifierTest
{
  private val workflow = {
    val workflowScript = """define workflow { execute executable="/SCRIPT.cmd", agent="/AGENT"; }"""
    WorkflowParser.parse(WorkflowPath("/WORKFLOW") ~ "1.0", workflowScript).orThrow
  }

  private val signerIds = SignerId("InventoryItemVerifierTest") :: Nil

  private val (signer, verifier) = {
    val password = SecretString("TEST-PASSWORD")
    val secretKey = PgpKeyGenerator.generateSecretKey(signerIds.head, password, keySize = 1024/*fast*/)
    val verifier = PgpSignatureVerifier.checked(
      bytesToInputStreamResource(secretKey.getPublicKey.toArmoredAsciiBytes) :: Nil
    ).orThrow
    val signer = PgpSigner(secretKey, password).orThrow
    (signer, verifier)
  }

  private implicit val jsonCodec = TypedJsonCodec[InventoryItem](
    Subtype(Workflow.jsonEncoder, Workflow.topJsonDecoder))
  private val itemSigner = new InventoryItemSigner(signer, jsonCodec)
  private val itemVerifier = new InventoryItemVerifier(verifier, jsonCodec)
}
