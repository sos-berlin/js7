package js7.tests.controller.commands

import java.nio.file.Files.{copy, createTempDirectory}
import js7.base.auth.{UserAndPassword, UserId}
import js7.base.circeutils.CirceUtils._
import js7.base.crypt.{GenericSignature, SignedString}
import js7.base.generic.SecretString
import js7.base.problem.Checked
import js7.base.problem.Checked.Ops
import js7.base.time.ScalaTime._
import js7.base.web.HttpClient
import js7.common.process.Processes.runProcess
import js7.common.scalautil.FileUtils.syntax._
import js7.common.scalautil.FileUtils.{deleteDirectoryRecursively, withTemporaryDirectory}
import js7.common.scalautil.MonixUtils.syntax.RichTask
import js7.controller.data.ControllerCommand
import js7.controller.data.ControllerCommand.UpdateRepo
import js7.core.crypt.x509.X509Signer
import js7.data.controller.ControllerItems._
import js7.data.item.{InventoryItem, VersionId}
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.controller.commands.UpdateRepoX509Test._
import js7.tests.testenv.ControllerAgentForScalaTest
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class UpdateRepoX509Test extends AnyFreeSpec with ControllerAgentForScalaTest
{
  protected val agentRefPaths = Nil
  protected val inventoryItems = Nil
  private lazy val (signer_, verifier_) = X509Signer.forTest()
  override protected def signer = signer_
  override protected def verifier = verifier_
  private lazy val workDir = createTempDirectory("UpdateRepoX509Test")
  private lazy val privateKeyFile = workDir / "test.key"
  private lazy val publicKeyFile = workDir / "test.pem"

  override def beforeAll() = {
    runProcess(s"openssl genrsa -out '$privateKeyFile' 1024")
    runProcess(s"openssl rsa -in '$privateKeyFile' -pubout -outform pem -out '$publicKeyFile'")
    copy(publicKeyFile, directoryProvider.controller.configDir / "private" / "trusted-x509-keys" / "test.pem")

    (directoryProvider.controller.configDir / "private" / "private.conf") ++=
     """js7.auth.users {
       |  UpdateRepoX509Test {
       |    password = "plain:TEST-PASSWORD"
       |    permissions = [ UpdateRepo ]
       |  }
       |}
       |""".stripMargin
    super.beforeAll()
  }

  override def afterAll() = {
    deleteDirectoryRecursively(workDir)
    super.afterAll()
  }

  "UpdateRepo with internally generated signature key" in {
    controller.httpApiDefaultLogin(Some(UserAndPassword(UserId("UpdateRepoX509Test"), SecretString("TEST-PASSWORD"))))
    controller.httpApi.login() await 99.s
    val v1 = VersionId("1")
    executeCommand(UpdateRepo(v1, sign(workflow withVersion v1) :: Nil)).orThrow
  }

  "UpdateRepo with openssl-generated signature key" in {
    withTemporaryDirectory("UpdateRepoX509Test-") { dir =>
      val itemFile = dir / "workflow.json"
      val signatureFile = dir / "workflow.json.signature"
      val signatureBase64File = dir / "workflow.json.signature.base64"
      val v2 = VersionId("2")

      itemFile := (workflow.withVersion(v2): InventoryItem)

      runProcess(s"openssl dgst -sign '$privateKeyFile' -sha512 -out '$signatureFile' '$itemFile'")
      runProcess(s"openssl base64 -in '$signatureFile' -out '$signatureBase64File'")
      runProcess(s"openssl dgst -verify '$publicKeyFile' -sha512 -signature '$signatureFile' '$itemFile'")

      val signedString = SignedString(
        itemFile.contentString,
        GenericSignature("X509", signatureBase64File.contentString))
      executeCommand(UpdateRepo(v2, signedString :: Nil)).orThrow
    }
  }

  private def executeCommand(cmd: ControllerCommand): Checked[cmd.Response] =
    HttpClient.liftProblem(controller.httpApi.executeCommand(cmd)) await 99.s
}

object UpdateRepoX509Test
{
  private val workflow = Workflow.of(WorkflowPath("/WORKFLOW"))
}
