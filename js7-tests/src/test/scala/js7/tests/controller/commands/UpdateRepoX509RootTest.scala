package js7.tests.controller.commands

import io.circe.syntax.EncoderOps
import java.nio.file.Files.{copy, createDirectory, createTempDirectory}
import js7.base.Problems.{MessageSignedByUnknownProblem, TamperedWithSignedMessageProblem}
import js7.base.circeutils.CirceUtils.*
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.crypt.x509.Openssl
import js7.base.crypt.{GenericSignature, Signed, SignedString}
import js7.base.io.file.FileUtils.deleteDirectoryRecursively
import js7.base.io.file.FileUtils.syntax.*
import js7.base.problem.Checked.Ops
import js7.base.test.OurTestSuite
import js7.base.thread.MonixBlocking.syntax.RichTask
import js7.base.time.ScalaTime.*
import js7.data.controller.ControllerState.signableItemJsonCodec
import js7.data.item.{SignableItem, VersionId}
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.controller.commands.UpdateRepoX509RootTest.*
import js7.tests.testenv.ControllerAgentForScalaTest
import monix.execution.Scheduler.Implicits.global

/**
  * @author Joacim Zschimmer
  */
final class UpdateRepoX509RootTest extends OurTestSuite with ControllerAgentForScalaTest
{
  protected val agentPaths = Nil
  protected val items = Nil
  //private lazy val (signer_, verifier_) = X509Signer.forTest()
  //override protected def verifier = verifier_
  private lazy val workDir = createTempDirectory("UpdateRepoX509RootTest")
  private lazy val openssl = new Openssl(workDir)
  private lazy val root = new openssl.Root("Root")

  protected override def controllerConfig = config"""
    js7.auth.users.TEST-USER.permissions = [ UpdateItem ]"""

  override def beforeAll() = {
    createDirectory(directoryProvider.controller.configDir / "private" / "trusted-x509-keys")
    copy(root.certificateFile,
      directoryProvider.controller.configDir / "private" / "trusted-x509-keys" / "Root.crt")

    (directoryProvider.controller.configDir / "private" / "private.conf") ++=
      s"""js7.configuration.trusted-signature-keys {
         |  X509 = $${js7.config-directory}"/private/trusted-x509-keys"
         |}
         |""".stripMargin
    super.beforeAll()
  }

  override def afterAll() = {
    deleteDirectoryRecursively(workDir)
    super.afterAll()
  }

  "UpdateRepo with openssl-generated root certificate" - {
    val v2 = VersionId("2")
    lazy val signer = new root.Signer("CERTIFIED-SIGNER")
    lazy val itemFile = workDir / "workflow.json"
    lazy val signatureFile = signer.sign(itemFile)

    "Signature matches item" in {
      val item: SignableItem = workflow.withVersion(v2)
      itemFile := item.asJson
      val signedString = SignedString.x509WithCertificate(
        itemFile.contentString,
        signatureFile.contentString,
        algorithm = "SHA512withRSA",
        signerCertificate = signer.certificateFile.contentString)
      val signed = Signed(item, signedString)
      controllerApi.updateRepo(v2, Seq(signed)).await(99.s).orThrow
    }

    "Signature does not match item (item tampered)" in {
      val v3 = VersionId("3")
      val item: SignableItem = workflow.withVersion(v3)
      itemFile := item.asJson
      val signedString = SignedString(
        itemFile.contentString + " ",
        GenericSignature("X509", signatureFile.contentString,
          algorithm = Some("SHA512withRSA"),
          signerCertificate = Some(signer.certificateFile.contentString)))
      val signed = Signed(item, signedString)
      assert(controllerApi.updateRepo(v3, Seq(signed)).await(99.s) == Left(TamperedWithSignedMessageProblem))
    }
  }

  "Add item with an unknown signer's certificate which controller fails to verify against installed root certificate" in {
    val v4 = VersionId("4")
    val itemFile = workDir / "workflow.json"
    val item: SignableItem = workflow.withVersion(v4)
    itemFile := item.asJson
    val alienRoot = new openssl.Root("ALIEN-ROOT")
    val alienSigner = new alienRoot.Signer("ALIEN")
    val alienSignatureFile = alienSigner.sign(itemFile)
    val alienSignedString = SignedString(
      itemFile.contentString,
      GenericSignature("X509", alienSignatureFile.contentString,
        algorithm = Some("SHA512withRSA"),
        signerCertificate = Some(alienSigner.certificateFile.contentString)))
    val alienSigned = Signed(item, alienSignedString)
    assert(controllerApi.updateRepo(v4, Seq(alienSigned)).await(99.s) == Left(MessageSignedByUnknownProblem))
  }
}

object UpdateRepoX509RootTest
{
  private val workflow = Workflow.of(WorkflowPath("WORKFLOW"))
}
