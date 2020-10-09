package js7.tests.controller.commands

import java.nio.file.Files.createTempDirectory
import js7.base.Problems.TamperedWithSignedMessageProblem
import js7.base.auth.UserId
import js7.base.circeutils.CirceUtils._
import js7.base.crypt.{GenericSignature, SignedString, SignerId}
import js7.base.generic.SecretString
import js7.base.problem.Checked.Ops
import js7.base.problem.{Checked, Problem}
import js7.base.time.ScalaTime._
import js7.base.web.HttpClient
import js7.common.process.Processes.runProcess
import js7.common.scalautil.FileUtils.syntax._
import js7.common.scalautil.FileUtils.{deleteDirectoryRecursively, withTemporaryDirectory}
import js7.common.scalautil.MonixUtils.syntax.RichTask
import js7.controller.data.ControllerCommand
import js7.controller.data.ControllerCommand.UpdateRepo
import js7.core.crypt.x509.X509SignatureVerifier
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
  override protected lazy val verifier = {
    runProcess(s"openssl req -x509 -sha512 -newkey rsa:1024 -days 2 -subj '/CN=SIGNER' -nodes" +
      s" -keyout '$privateKeyFile' -out '$certificateFile'")
    X509SignatureVerifier.checked(Seq(certificateFile.byteArray), "UpdateRepoX509Test").orThrow
  }

  private lazy val workDir = createTempDirectory("UpdateRepoX509Test")
  private lazy val privateKeyFile = workDir / "test.key"
  private lazy val certificateFile = workDir / "test.pem"

  override def beforeAll() = {
    (directoryProvider.controller.configDir / "private" / "private.conf") ++=
     """js7.auth.users {
       |  UpdateRepoX509Test {
       |    password = "plain:TEST-PASSWORD"
       |    permissions = [ UpdateRepo ]
       |  }
       |}
       |""".stripMargin
    super.beforeAll()
    controller.httpApiDefaultLogin(Some(UserId("UpdateRepoX509Test") -> SecretString("TEST-PASSWORD")))
    controller.httpApi.login() await 99.s
  }

  override def afterAll() = {
    deleteDirectoryRecursively(workDir)
    super.afterAll()
  }

  //"UpdateRepo with internally generated signature" in {
  //  val v1 = VersionId("1")
  //  executeCommand(UpdateRepo(v1, sign(workflow withVersion v1) :: Nil)).orThrow
  //}

  "UpdateRepo with openssl-generated signature" in {
    withTemporaryDirectory("UpdateRepoX509Test-") { dir =>
      val itemFile = dir / "workflow.json"
      val signatureFile = dir / "workflow.json.signature"
      val signatureBase64File = dir / "workflow.json.signature.base64"
      val publicKeyFile = dir / "test.pem"
      val v2 = VersionId("2")

      itemFile := (workflow.withVersion(v2): InventoryItem)

      runProcess(s"openssl dgst -sign '$privateKeyFile' -sha512 -out '$signatureFile' '$itemFile'")
      runProcess(s"openssl base64 -in '$signatureFile' -out '$signatureBase64File'")

      // Verifiy with openssl
      runProcess(s"sh -c 'openssl x509 -pubkey -noout -in \'$certificateFile\' >\'$publicKeyFile\''")
      runProcess(s"openssl dgst -verify '$publicKeyFile' -sha512 -signature '$signatureFile' '$itemFile'")

      val signedString = SignedString(
        itemFile.contentString,
        GenericSignature("X509", signatureBase64File.contentString, algorithm = Some("SHA512withRSA"), signerId = Some(SignerId("CN=SIGNER"))))
      executeCommand(UpdateRepo(v2, signedString :: Nil)).orThrow

      locally {
        val v3 = VersionId("3")
        itemFile := (workflow.withVersion(v3): InventoryItem)
        runProcess(s"openssl dgst -sign '$privateKeyFile' -sha512 -out '$signatureFile' '$itemFile'")
        runProcess(s"openssl base64 -in '$signatureFile' -out '$signatureBase64File'")
        val signedStringV3 = SignedString(
          itemFile.contentString + "-TAMPERED",
          GenericSignature("X509", signatureBase64File.contentString, algorithm = Some("SHA512withRSA"), signerId = Some(SignerId("CN=SIGNER"))))
        assert(executeCommand(UpdateRepo(v3, signedStringV3 :: Nil)) == Left(TamperedWithSignedMessageProblem))
      }

      locally {
        val v4 = VersionId("4")
        itemFile := (workflow.withVersion(v4): InventoryItem)
        runProcess(s"openssl dgst -sign '$privateKeyFile' -sha512 -out '$signatureFile' '$itemFile'")
        runProcess(s"openssl base64 -in '$signatureFile' -out '$signatureBase64File'")
        val signedStringV4 = SignedString(
          itemFile.contentString,
          GenericSignature("X509", signatureBase64File.contentString, algorithm = Some("SHA512withRSA"), signerId = Some(SignerId("CN=ALIEN"))))
        assert(executeCommand(UpdateRepo(v4, signedStringV4 :: Nil)) ==
          Left(Problem("The signature's SignerId 'CN=ALIEN' is unknown")))
      }
    }
  }

  private def executeCommand(cmd: ControllerCommand): Checked[cmd.Response] =
    HttpClient.liftProblem(controller.httpApi.executeCommand(cmd)) await 99.s
}

object UpdateRepoX509Test
{
  private val workflow = Workflow.of(WorkflowPath("/WORKFLOW"))
}
