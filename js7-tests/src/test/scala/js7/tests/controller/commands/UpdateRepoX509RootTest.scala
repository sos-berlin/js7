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
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.data.controller.ControllerState.signableItemJsonCodec
import js7.data.item.{SignableItem, VersionId}
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.controller.commands.UpdateRepoX509RootTest.*
import js7.tests.testenv.ControllerAgentForScalaTest

/**
  * @author Joacim Zschimmer
  */
final class UpdateRepoX509RootTest extends OurTestSuite, ControllerAgentForScalaTest:

  protected val agentPaths = Nil
  protected val items = Nil
  //private lazy val (signer_, verifier_) = X509Signer.forTest()
  //override protected def verifier = verifier_
  private lazy val workDir = createTempDirectory("UpdateRepoX509RootTest")
  private lazy val openssl = new Openssl(workDir)
  private lazy val root = new openssl.Root("Root")

  protected override def controllerConfig = config"""
    js7.auth.users.TEST-USER.permissions = [ UpdateItem ]"""

  override def beforeAll() =
    createDirectory(directoryProvider.controllerEnv.configDir / "private" / "trusted-x509-keys")
    copy(root.certificateFile,
      directoryProvider.controllerEnv.configDir / "private" / "trusted-x509-keys" / "Root.crt")

    directoryProvider.controllerEnv.privateConf ++=
      s"""js7.configuration.trusted-signature-keys {
         |  X509 = $${js7.config-directory}"/private/trusted-x509-keys"
         |}
         |""".stripMargin
    super.beforeAll()

  override def afterAll() =
    try
      deleteDirectoryRecursively(workDir)
    finally
      super.afterAll()

  "UpdateRepo with openssl-generated root certificate" - {
    val v2 = VersionId("2")
    lazy val signer = new root.Signer("CERTIFIED-SIGNER")
    lazy val itemFile = workDir / "workflow.json"
    lazy val signatureFile = signer.signFile(itemFile)

    "Signature matches item" in:
      val item: SignableItem = workflow.withVersion(v2)
      itemFile := item.asJson
      val signedString = SignedString.x509WithCertificate(
        itemFile.contentString,
        signatureFile.contentString,
        algorithm = "SHA512withRSA",
        signerCertificate = signer.certificateString)
      val signed = Signed(item, signedString)
      controller.api.updateRepo(v2, Seq(signed)).await(99.s).orThrow

    "Signature does not match item (item tampered)" in:
      val v3 = VersionId("3")
      val item: SignableItem = workflow.withVersion(v3)
      itemFile := item.asJson
      val signedString = SignedString(
        itemFile.contentString + " ",
        GenericSignature("X509", signatureFile.contentString,
          algorithm = Some("SHA512withRSA"),
          signerCertificate = Some(signer.certificateString)))
      val signed = Signed(item, signedString)
      assert(controller.api.updateRepo(v3, Seq(signed)).await(99.s) == Left(TamperedWithSignedMessageProblem))
  }

  "Add item with an unknown signer's certificate which controller fails to verify against installed root certificate" in:
    val v4 = VersionId("4")
    val itemFile = workDir / "workflow.json"
    val item: SignableItem = workflow.withVersion(v4)
    itemFile := item.asJson
    val alienRoot = new openssl.Root("ALIEN-ROOT")
    val alienSigner = new alienRoot.Signer("ALIEN")
    val alienSignatureFile = alienSigner.signFile(itemFile)
    val alienSignedString = SignedString(
      itemFile.contentString,
      GenericSignature("X509", alienSignatureFile.contentString,
        algorithm = Some("SHA512withRSA"),
        signerCertificate = Some(alienSigner.certificateString)))
    val alienSigned = Signed(item, alienSignedString)
    assert(controller.api.updateRepo(v4, Seq(alienSigned)).await(99.s) == Left(MessageSignedByUnknownProblem))


object UpdateRepoX509RootTest:

  private val workflow = Workflow.of(WorkflowPath("WORKFLOW"))
