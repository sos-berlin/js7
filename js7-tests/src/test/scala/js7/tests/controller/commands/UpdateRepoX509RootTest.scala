package js7.tests.controller.commands

import java.nio.file.Files.{copy, createDirectory, createTempDirectory}
import js7.base.Problems.{MessageSignedByUnknownProblem, TamperedWithSignedMessageProblem}
import js7.base.auth.UserId
import js7.base.circeutils.CirceUtils._
import js7.base.crypt.{GenericSignature, SignedString}
import js7.base.generic.SecretString
import js7.base.problem.Checked
import js7.base.problem.Checked.Ops
import js7.base.time.ScalaTime._
import js7.base.web.HttpClient
import js7.common.scalautil.FileUtils.deleteDirectoryRecursively
import js7.common.scalautil.FileUtils.syntax._
import js7.common.scalautil.MonixUtils.syntax.RichTask
import js7.controller.data.ControllerCommand
import js7.controller.data.ControllerCommand.UpdateRepo
import js7.core.crypt.x509.OpensslContext
import js7.data.controller.ControllerItems._
import js7.data.item.{InventoryItem, VersionId}
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
  protected val agentRefPaths = Nil
  protected val inventoryItems = Nil
  //private lazy val (signer_, verifier_) = X509Signer.forTest()
  //override protected def verifier = verifier_
  private lazy val workDir = createTempDirectory("UpdateRepoX509RootTest")
  private lazy val openssl = new OpensslContext(workDir)
  private lazy val root = new openssl.Root("Root")

  override def beforeAll() = {
    createDirectory(directoryProvider.controller.configDir / "private" / "trusted-x509-keys")
    copy(root.certificateFile,
      directoryProvider.controller.configDir / "private" / "trusted-x509-keys" / "Root.crt")

    (directoryProvider.controller.configDir / "private" / "private.conf") ++=
      s"""js7.configuration.trusted-signature-keys {
         |  X509 = $${js7.config-directory}"/private/trusted-x509-keys"
         |}
         |js7.auth.users {
         |  UpdateRepoX509RootTest {
         |    password = "plain:TEST-PASSWORD"
         |    permissions = [ UpdateRepo ]
         |  }
         |}
         |""".stripMargin
    super.beforeAll()

    controller.httpApiDefaultLogin(Some(UserId("UpdateRepoX509RootTest") -> SecretString("TEST-PASSWORD")))
    controller.httpApi.login() await 99.s
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
      itemFile := (workflow.withVersion(v2): InventoryItem)
      val signedString = SignedString(
        itemFile.contentString,
        GenericSignature("X509", signatureFile.contentString,
          algorithm = Some("SHA512withRSA"),
          signerCertificate = Some(signer.certificateFile.contentString)))
      executeCommand(UpdateRepo(v2, signedString :: Nil)).orThrow
    }

    "Signature does not match item (item tampered)" in {
      val v3 = VersionId("3")
      itemFile := (workflow.withVersion(v3): InventoryItem)
      val signedString = SignedString(
        itemFile.contentString + "-TAMPERED",
        GenericSignature("X509", signatureFile.contentString,
          algorithm = Some("SHA512withRSA"),
          signerCertificate = Some(signer.certificateFile.contentString)))
      assert(executeCommand(UpdateRepo(v3, signedString :: Nil)) == Left(TamperedWithSignedMessageProblem))
    }
  }

  "Add item with an unknown signer's certificate which controller fails to verify against installed root certificate" in {
    val v4 = VersionId("4")
    val itemFile = workDir / "workflow.json"
    itemFile := (workflow.withVersion(v4): InventoryItem)
    val alienRoot = new openssl.Root("ALIEN-ROOT")
    val alienSigner = new alienRoot.Signer("ALIEN")
    val alienSignatureFile = alienSigner.sign(itemFile)
    val alienSignedString = SignedString(
      itemFile.contentString,
      GenericSignature("X509", alienSignatureFile.contentString,
        algorithm = Some("SHA512withRSA"),
        signerCertificate = Some(alienSigner.certificateFile.contentString)))
    assert(executeCommand(UpdateRepo(v4, alienSignedString :: Nil)) == Left(MessageSignedByUnknownProblem))
  }

  private def executeCommand(cmd: ControllerCommand): Checked[cmd.Response] =
    HttpClient.liftProblem(controller.httpApi.executeCommand(cmd)) await 99.s
}

object UpdateRepoX509RootTest
{
  private val workflow = Workflow.of(WorkflowPath("/WORKFLOW"))
}
