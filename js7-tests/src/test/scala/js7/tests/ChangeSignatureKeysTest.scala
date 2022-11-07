package js7.tests

import io.circe.syntax.EncoderOps
import java.io.FileOutputStream
import java.nio.file.Files.{copy, createDirectory, createTempDirectory}
import js7.agent.scheduler.AgentActor
import js7.base.Problems.{MessageSignedByUnknownProblem, TamperedWithSignedMessageProblem}
import js7.base.circeutils.CirceUtils.*
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.crypt.x509.Openssl
import js7.base.crypt.{GenericSignature, Signed, SignedString}
import js7.base.io.file.FileUtils.deleteDirectoryRecursively
import js7.base.io.file.FileUtils.syntax.*
import js7.base.io.file.watch.BasicDirectoryWatcher.systemWatchDelay
import js7.base.problem.Checked.Ops
import js7.base.test.OurTestSuite
import js7.base.thread.Futures.implicits.SuccessFuture
import js7.base.thread.MonixBlocking.syntax.RichTask
import js7.base.time.ScalaTime.*
import js7.base.utils.AutoClosing.autoClosing
import js7.controller.RunningController
import js7.data.agent.AgentPath
import js7.data.controller.ControllerState.signableItemJsonCodec
import js7.data.item.{SignableItem, VersionId}
import js7.data.order.OrderEvent.OrderFinished
import js7.data.order.{FreshOrder, OrderId}
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.ChangeSignatureKeysTest.*
import js7.tests.jobs.EmptyJob
import js7.tests.testenv.ControllerAgentForScalaTest
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global

final class ChangeSignatureKeysTest extends OurTestSuite with ControllerAgentForScalaTest
{
  protected val agentPaths = Seq(agentPath)
  protected val items = Nil
  private lazy val workDir = createTempDirectory("ChangeSignatureKeysTest")
  private lazy val openssl = new Openssl(workDir)
  private lazy val root = new openssl.Root("Root")
  private lazy val controllersKeyDirectory =
    directoryProvider.controller.configDir / "private" / "trusted-x509-keys"
  private lazy val agentsKeyDirectory =
    directoryProvider.agents.head.configDir / "private" / "trusted-x509-keys"

  override protected def controllerConfig = config"""
    js7.auth.users.TEST-USER.permissions = [ UpdateItem ]
    js7.configuration.trusted-signature-key-settings.file-delay = ${(systemWatchDelay + 1.s).pretty}
    """

  override protected def agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = on
    """

  override def beforeAll() = {
    createDirectory(controllersKeyDirectory)
    copy(root.certificateFile, controllersKeyDirectory / "Root.crt")
    (directoryProvider.controller.configDir / "private" / "private.conf") ++=
      s"""js7.configuration.trusted-signature-keys {
         |  X509 = "$controllersKeyDirectory"
         |}
         |""".stripMargin

    createDirectory(agentsKeyDirectory)
    copy(root.certificateFile, agentsKeyDirectory / "Root.crt")
    (directoryProvider.agents.head.configDir / "private" / "private.conf") ++=
      s"""js7.configuration.trusted-signature-keys {
         |  X509 = "$agentsKeyDirectory"
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

    "Signature matches item" in {
      val item: SignableItem = workflow.withVersion(v2)
      val itemJson = item.asJson.compactPrint
      val signedString = SignedString.x509WithCertificate(
        itemJson,
        signer.signString(itemJson),
        algorithm = "SHA512withRSA",
        signerCertificate = signer.certificateString)
      val signed = Signed(item, signedString)
      controllerApi.updateRepo(v2, Seq(signed)).await(99.s).orThrow

      val events = controller.runOrder(FreshOrder(OrderId("2"), workflow.path))
        .map(_.value)
      assert(events.last.isInstanceOf[OrderFinished])
    }

    "Signature does not match item (item tampered)" in {
      val v3 = VersionId("3")
      val item: SignableItem = workflow.withVersion(v3)
      val itemJson = item.asJson.compactPrint
      val signedString = SignedString(
        itemJson + " ",
        GenericSignature("X509", signer.signString(itemJson),
          algorithm = Some("SHA512withRSA"),
          signerCertificate = Some(signer.certificateString)))
      val signed = Signed(item, signedString)
      assert(controllerApi.updateRepo(v3, Seq(signed)).await(99.s) ==
        Left(TamperedWithSignedMessageProblem))
    }
  }

  "Add item with a changed signer's certificate" - {
    lazy val changedRoot = new openssl.Root("CHANGED-ROOT")
    lazy val v4 = VersionId("4")
    lazy val item: SignableItem = workflow.withVersion(v4)
    lazy val itemJson = item.asJson.compactPrint
    lazy val changedSigner = new changedRoot.Signer("CHANGED")
    lazy val changedSignedString = SignedString(
      itemJson,
      GenericSignature("X509", changedSigner.signString(itemJson),
        algorithm = Some("SHA512withRSA"),
        signerCertificate = Some(changedSigner.certificateString)))
    lazy val changedSigned = Signed(item, changedSignedString)

    "Controller fails to verify against installed root certificate" in {
      assert(controllerApi.updateRepo(v4, Seq(changedSigned)).await(99.s) ==
        Left(MessageSignedByUnknownProblem))
    }

    "Change changed root certificate in configuration directory, do it slowly" in {
      val whenUpdated = Task.parZip2(
        controller.testEventBus.when[RunningController.ItemSignatureKeysUpdated.type],
        agent.testEventBus.when[AgentActor.ItemSignatureKeysUpdated.type]
      ).runToFuture

      // See also js7.conf:
      // js7.configuration.trusted-signature-key-settings.file-delay = 2s
      val cert = changedRoot.certificateFile.contentBytes
      autoClosing(new FileOutputStream((controllersKeyDirectory / "Root.crt").toFile)) { controllerFile =>
        autoClosing(new FileOutputStream((agentsKeyDirectory / "Root.crt").toFile)) { agentFile =>
          val n = 40
          for (i <- 0 until n) withClue(s"${(i.s / 10).pretty} -> ") {
            controllerFile.write(cert(i))
            controllerFile.flush()
            agentFile.write(cert(i))
            agentFile.flush()
            sleep(100.ms)
            assert(!whenUpdated.isCompleted)
          }
          controllerFile.write(cert, n, cert.length - n)
          agentFile.write(cert, n, cert.length - n)
        }
      }
      whenUpdated.await(99.s)

      controllerApi.updateRepo(v4, Seq(changedSigned)).await(99.s).orThrow

      val events = controller.runOrder(FreshOrder(OrderId("4"), workflow.path))
        .map(_.value)
      assert(events.last.isInstanceOf[OrderFinished])
    }
  }
}

object ChangeSignatureKeysTest
{
  private val agentPath = AgentPath("AGENT")
  private val workflow = Workflow(
    WorkflowPath("WORKFLOW"),
    Seq(
      EmptyJob.execute(agentPath)))
}
