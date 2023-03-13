package js7.tests.controller.commands

import io.circe.syntax.EncoderOps
import java.nio.file.Files.createTempDirectory
import js7.base.Problems.TamperedWithSignedMessageProblem
import js7.base.circeutils.CirceUtils.*
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.crypt.x509.Openssl.{openssl, quote}
import js7.base.crypt.x509.X509SignatureVerifier
import js7.base.crypt.{GenericSignature, Signed, SignedString, SignerId}
import js7.base.io.file.FileUtils.syntax.*
import js7.base.io.file.FileUtils.{deleteDirectoryRecursively, withTemporaryDirectory}
import js7.base.io.process.Processes.runProcess
import js7.base.problem.Checked.Ops
import js7.base.problem.Problem
import js7.base.test.OurTestSuite
import js7.base.thread.MonixBlocking.syntax.RichTask
import js7.base.time.ScalaTime.*
import js7.data.controller.ControllerState.{signableItemJsonCodec, versionedItemJsonCodec}
import js7.data.item.{SignableItem, VersionId, VersionedItem}
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.controller.commands.UpdateRepoX509Test.*
import js7.tests.testenv.ControllerAgentForScalaTest
import monix.execution.Scheduler.Implicits.traced

/**
  * @author Joacim Zschimmer
  */
final class UpdateRepoX509Test extends OurTestSuite with ControllerAgentForScalaTest
{
  protected val agentPaths = Nil
  protected val items = Nil
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

      runProcess(s"$openssl dgst -sign ${quote(privateKeyFile)} -sha512 -out ${quote(signatureFile)} ${quote(itemFile)}")
      runProcess(s"$openssl base64 -in ${quote(signatureFile)} -out ${quote(signatureBase64File)}")

      // Verifiy with openssl
      runProcess(s"""sh -c "openssl x509 -pubkey -noout -in ${quote(certificateFile)} >${quote(publicKeyFile)}"""")
      runProcess(s"$openssl dgst -verify ${quote(publicKeyFile)} -sha512 -signature ${quote(signatureFile)} ${quote(itemFile)}")

      val signedString = SignedString.x509WithSignerId/*Java API*/(
        itemFile.contentString,
        signatureBase64File.contentString,
        algorithm = "SHA512withRSA",
        signerId = SignerId.of("CN=SIGNER"))
      val signed = Signed(itemFile.contentString.parseJsonAs[SignableItem].orThrow, signedString)
      controller.api.updateRepo(v2, Seq(signed)).await(99.s).orThrow

      locally {
        val v3 = VersionId("3")
        itemFile := (workflow.withVersion(v3): VersionedItem).asJson
        runProcess(s"$openssl dgst -sign ${quote(privateKeyFile)} -sha512 -out ${quote(signatureFile)} ${quote(itemFile)}")
        runProcess(s"$openssl base64 -in ${quote(signatureFile)} -out ${quote(signatureBase64File)}")
        val signedStringV3 = SignedString(
          itemFile.contentString + " ",
          GenericSignature("X509", signatureBase64File.contentString, algorithm = Some("SHA512withRSA"), signerId = Some(SignerId("CN=SIGNER"))))
        val signed = Signed(itemFile.contentString.parseJsonAs[SignableItem].orThrow, signedStringV3)
        assert(controller.api.updateRepo(v3, Seq(signed)).await(99.s) == Left(TamperedWithSignedMessageProblem))
      }

      locally {
        val v4 = VersionId("4")
        itemFile := (workflow.withVersion(v4): VersionedItem).asJson
        runProcess(s"$openssl dgst -sign ${quote(privateKeyFile)} -sha512 -out ${quote(signatureFile)} ${quote(itemFile)}")
        runProcess(s"$openssl base64 -in ${quote(signatureFile)} -out ${quote(signatureBase64File)}")
        val signedStringV4 = SignedString(
          itemFile.contentString,
          GenericSignature("X509", signatureBase64File.contentString, algorithm = Some("SHA512withRSA"), signerId = Some(SignerId("CN=ALIEN"))))
        val signed = Signed(itemFile.contentString.parseJsonAs[SignableItem].orThrow, signedStringV4)
        assert(controller.api.updateRepo(v4, Seq(signed)).await(99.s) == Left(Problem("The signature's SignerId is unknown: CN=ALIEN")))
      }
    }
  }
}

object UpdateRepoX509Test
{
  private val workflow = Workflow.of(WorkflowPath("WORKFLOW"))
}
