package js7.tests.controller.commands

import java.nio.file.Files.createTempDirectory
import js7.base.Problems.TamperedWithSignedMessageProblem
import js7.base.circeutils.CirceUtils._
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.crypt.x509.Openssl.openssl
import js7.base.crypt.x509.X509SignatureVerifier
import js7.base.crypt.{GenericSignature, SignedString, SignerId}
import js7.base.io.file.FileUtils.syntax._
import js7.base.io.file.FileUtils.{deleteDirectoryRecursively, withTemporaryDirectory}
import js7.base.io.process.Processes.runProcess
import js7.base.problem.Checked.Ops
import js7.base.problem.Problem
import js7.base.thread.MonixBlocking.syntax.RichTask
import js7.base.time.ScalaTime._
import js7.data.controller.ControllerState.versionedItemJsonCodec
import js7.data.item.{VersionId, VersionedItem}
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
  protected val agentIds = Nil
  protected val versionedItems = Nil
  override protected lazy val verifier = {
    runProcess(s"$openssl req -x509 -newkey rsa:1024 -sha512 -days 2 -nodes -subj '/CN=SIGNER'" +
      s" -keyout '$privateKeyFile' -out '$certificateFile'")
    X509SignatureVerifier.checked(Seq(certificateFile.byteArray), "UpdateRepoX509Test").orThrow
  }

  private lazy val workDir = createTempDirectory("UpdateRepoX509Test")
  private lazy val privateKeyFile = workDir / "test.key"
  private lazy val certificateFile = workDir / "test.pem"

  override def controllerConfig = config"""js7.auth.users.TEST-USER.permissions = [ UpdateItem ]"""

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

      itemFile := (workflow.withVersion(v2): VersionedItem)

      runProcess(s"$openssl dgst -sign '$privateKeyFile' -sha512 -out '$signatureFile' '$itemFile'")
      runProcess(s"$openssl base64 -in '$signatureFile' -out '$signatureBase64File'")

      // Verifiy with openssl
      runProcess(s"sh -c 'openssl x509 -pubkey -noout -in \'$certificateFile\' >\'$publicKeyFile\''")
      runProcess(s"$openssl dgst -verify '$publicKeyFile' -sha512 -signature '$signatureFile' '$itemFile'")

      val signedString = SignedString.x509WithSignerId/*Java API*/(
        itemFile.contentString,
        signatureBase64File.contentString,
        algorithm = "SHA512withRSA",
        signerId = SignerId.of("CN=SIGNER"))
      controllerApi.updateRepo(v2, Seq(signedString))
        .await(99.s).orThrow

      locally {
        val v3 = VersionId("3")
        itemFile := (workflow.withVersion(v3): VersionedItem)
        runProcess(s"$openssl dgst -sign '$privateKeyFile' -sha512 -out '$signatureFile' '$itemFile'")
        runProcess(s"$openssl base64 -in '$signatureFile' -out '$signatureBase64File'")
        val signedStringV3 = SignedString(
          itemFile.contentString + "-TAMPERED",
          GenericSignature("X509", signatureBase64File.contentString, algorithm = Some("SHA512withRSA"), signerId = Some(SignerId("CN=SIGNER"))))
        assert(controllerApi.updateRepo(v3, Seq(signedStringV3)).await(99.s) == Left(TamperedWithSignedMessageProblem))
      }

      locally {
        val v4 = VersionId("4")
        itemFile := (workflow.withVersion(v4): VersionedItem)
        runProcess(s"$openssl dgst -sign '$privateKeyFile' -sha512 -out '$signatureFile' '$itemFile'")
        runProcess(s"$openssl base64 -in '$signatureFile' -out '$signatureBase64File'")
        val signedStringV4 = SignedString(
          itemFile.contentString,
          GenericSignature("X509", signatureBase64File.contentString, algorithm = Some("SHA512withRSA"), signerId = Some(SignerId("CN=ALIEN"))))
        assert(controllerApi.updateRepo(v4, Seq(signedStringV4)).await(99.s) == Left(Problem("The signature's SignerId is unknown: CN=ALIEN")))
      }
    }
  }
}

object UpdateRepoX509Test
{
  private val workflow = Workflow.of(WorkflowPath("WORKFLOW"))
}
