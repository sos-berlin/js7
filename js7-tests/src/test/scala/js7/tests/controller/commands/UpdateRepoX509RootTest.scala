package js7.tests.controller.commands

import java.nio.file.Files.{copy, createDirectory, createTempDirectory}
import js7.base.Problems.{MessageSignedByUnknownProblem, TamperedWithSignedMessageProblem}
import js7.base.circeutils.CirceUtils._
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.crypt.x509.Openssl
import js7.base.crypt.{GenericSignature, SignedString}
import js7.base.io.file.FileUtils.deleteDirectoryRecursively
import js7.base.io.file.FileUtils.syntax._
import js7.base.problem.Checked.Ops
import js7.base.thread.MonixBlocking.syntax.RichTask
import js7.base.time.ScalaTime._
import js7.data.controller.ControllerState.versionedItemJsonCodec
import js7.data.item.{VersionId, VersionedItem}
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.controller.commands.UpdateRepoX509RootTest._
import js7.tests.testenv.ControllerAgentForScalaTest
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class UpdateRepoX509RootTest extends AnyFreeSpec with ControllerAgentForScalaTest
{
  protected val agentIds = Nil
  protected val versionedItems = Nil
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
      itemFile := (workflow.withVersion(v2): VersionedItem)
      val signedString = SignedString.x509WithCertificate(
        itemFile.contentString,
        signatureFile.contentString,
        algorithm = "SHA512withRSA",
        signerCertificate = signer.certificateFile.contentString)
      controllerApi.updateRepo(v2, Seq(signedString)).await(99.s).orThrow
    }

    "Signature does not match item (item tampered)" in {
      val v3 = VersionId("3")
      itemFile := (workflow.withVersion(v3): VersionedItem)
      val signedString = SignedString(
        itemFile.contentString + "-TAMPERED",
        GenericSignature("X509", signatureFile.contentString,
          algorithm = Some("SHA512withRSA"),
          signerCertificate = Some(signer.certificateFile.contentString)))
      assert(controllerApi.updateRepo(v3, Seq(signedString)).await(99.s) == Left(TamperedWithSignedMessageProblem))
    }
  }

  "Add item with an unknown signer's certificate which controller fails to verify against installed root certificate" in {
    val v4 = VersionId("4")
    val itemFile = workDir / "workflow.json"
    itemFile := (workflow.withVersion(v4): VersionedItem)
    val alienRoot = new openssl.Root("ALIEN-ROOT")
    val alienSigner = new alienRoot.Signer("ALIEN")
    val alienSignatureFile = alienSigner.sign(itemFile)
    val alienSignedString = SignedString(
      itemFile.contentString,
      GenericSignature("X509", alienSignatureFile.contentString,
        algorithm = Some("SHA512withRSA"),
        signerCertificate = Some(alienSigner.certificateFile.contentString)))
    assert(controllerApi.updateRepo(v4, Seq(alienSignedString)).await(99.s) == Left(MessageSignedByUnknownProblem))
  }
}

object UpdateRepoX509RootTest
{
  private val workflow = Workflow.of(WorkflowPath("WORKFLOW"))
}
