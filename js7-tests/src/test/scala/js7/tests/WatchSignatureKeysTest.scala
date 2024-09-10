package js7.tests

import cats.effect.IO
import java.io.FileOutputStream
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Files.{createTempDirectory, delete}
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.crypt.SignerId
import js7.base.crypt.x509.X509Algorithm.SHA512withRSA
import js7.base.crypt.x509.{Openssl, X509Cert, X509SignatureVerifier, X509Signer}
import js7.base.data.ByteArray
import js7.base.io.file.FileUtils.deleteDirectoryRecursively
import js7.base.io.file.FileUtils.syntax.*
import js7.base.io.file.watch.BasicDirectoryWatch.systemWatchDelay
import js7.base.log.Logger
import js7.base.problem.Checked.Ops
import js7.base.problem.Problem
import js7.base.test.OurTestSuite
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.thread.Futures.implicits.SuccessFuture
import js7.base.time.ScalaTime.*
import js7.base.utils.AutoClosing.autoClosing
import js7.base.utils.CatsUtils.syntax.logWhenItTakesLonger
import js7.base.utils.Labeled
import js7.common.utils.FreeTcpPortFinder.findFreeLocalUri
import js7.controller.RunningController
import js7.data.agent.AgentPath
import js7.data.controller.ControllerState
import js7.data.item.{ItemSigner, VersionId}
import js7.data.order.OrderEvent.{OrderAdded, OrderFinished, OrderProcessingStarted}
import js7.data.order.{FreshOrder, OrderId}
import js7.data.subagent.{SubagentId, SubagentItem}
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.subagent.Subagent
import js7.tests.WatchSignatureKeysTest.*
import js7.tests.jobs.EmptyJob
import js7.tests.testenv.ControllerAgentForScalaTest
import js7.tests.testenv.DirectoryProvider.toLocalSubagentId
import scala.collection.View
import scala.concurrent.{ExecutionContext, Future}

final class WatchSignatureKeysTest extends OurTestSuite, ControllerAgentForScalaTest:

  private given ExecutionContext = executionContext
  override protected def controllerConfig = config"""
    js7.auth.users.TEST-USER.permissions = [ UpdateItem ]
    js7.directory-watch.watch-delay = 0s
    js7.directory-watch.directory-silence = ${(systemWatchDelay + 1.s).pretty}
    """

  // Used for Subagent, too
  override protected def agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = on
    js7.directory-watch.watch-delay = 0s
    js7.directory-watch.directory-silence = ${(systemWatchDelay + 1.s).pretty}
    """

  protected val agentPaths = Seq(agentPath)
  override protected lazy val bareSubagentItems = Seq(
    SubagentItem(bareSubagentId, agentPath, findFreeLocalUri()))
  protected val items = Nil

  private lazy val workDir = createTempDirectory("WatchSignatureKeysTest")
  private lazy val openssl = new Openssl(workDir)
  private lazy val aCertAndKey = openssl
    .generateCertWithPrivateKey("TEST", s"/$aSignerId")
    .orThrow
  override protected lazy val signer =
    X509Signer.checked(aCertAndKey.privateKey, SHA512withRSA, aSignerId).orThrow
  private lazy val itemSigner = new ItemSigner(signer, ControllerState.signableItemJsonCodec)
  override protected lazy val verifier =
    X509SignatureVerifier.checked(Seq(Labeled(aCertAndKey.certificate, "verifier"))).orThrow

  private lazy val controllersKeyDirectory =
    directoryProvider.controllerEnv.configDir / "private" / "trusted-x509-keys"
  private lazy val agentsKeyDirectory =
    directoryProvider.agentEnvs.head.configDir / "private" / "trusted-x509-keys"
  private lazy val subagentsKeyDirectory = directoryProvider
    .bareSubagentToDirectory(bareSubagentId) / "config" / "private" / "trusted-x509-keys"

  private val nextVersion = Iterator.from(1).map(i => VersionId(s"V$i")).next _

  override def beforeAll() =
    super.beforeAll()
    // Use only our BareSubagent
    enableSubagents(toLocalSubagentId(agentPath) -> false)

  override def afterAll() =
    deleteDirectoryRecursively(workDir)
    super.afterAll()

  "Signature matches item" in:
    val v = nextVersion()
    val item = workflow.withVersion(v)
    controller.api.updateRepo(v, Seq(itemSigner.sign(item))).await(99.s).orThrow
    testOrder(v)

  "Delete signature PEM file at Controller" in:
    val pem = (controllersKeyDirectory / "key-1.pem").byteArray

    locally:
      val whenUpdated = whenControllerAndAgentUpdated()

      delete(controllersKeyDirectory / "key-1.pem")
      delete(agentsKeyDirectory / "key-1.pem")
      delete(subagentsKeyDirectory / "key-1.pem")
      whenUpdated.await(99.s)

      val v = nextVersion()
      val checked = controller.api.updateRepo(v, Seq(sign(workflow.withVersion(v)))).await(99.s)
      assert(checked == Left(Problem(
        "The signature's SignerId is unknown: CN=WatchSignatureKeysTest-A")))

    locally:
      val whenUpdated = whenControllerAndAgentUpdated()
      controllersKeyDirectory / "key-1.pem" := pem
      agentsKeyDirectory / "key-1.pem" := pem
      subagentsKeyDirectory / "key-1.pem" := pem
      whenUpdated.await(99.s)

  "PEM file restored, Signature matches item" in:
    val v = nextVersion()
    val item = workflow.withVersion(v)
    controller.api.updateRepo(v, Seq(itemSigner.sign(item))).await(99.s).orThrow
    testOrder(v)

  "Change signature PEM file at Controller" - {
    val signerId = SignerId("CN=WatchSignatureKeysTest-B")
    lazy val bCertAndKey = openssl
      .generateCertWithPrivateKey("TEST", s"/$signerId")
      .orThrow
    lazy val bItemSigner =
      val bSigner = X509Signer.checked(bCertAndKey.privateKey, SHA512withRSA, signerId).orThrow
      new ItemSigner(bSigner, ControllerState.signableItemJsonCodec)

    "Item signed with previous signature key is rejected" in:
      val v = nextVersion()
      val whenUpdated = whenControllerAndAgentUpdated()

      X509Cert.fromPem(bCertAndKey.certificatePem).orThrow
      controllersKeyDirectory / "key-1.pem" := bCertAndKey.certificatePem
      agentsKeyDirectory / "key-1.pem" := bCertAndKey.certificatePem
      subagentsKeyDirectory / "key-1.pem" := bCertAndKey.certificatePem
      whenUpdated.await(99.s)

      val checked = controller.api.updateRepo(v, Seq(sign(workflow.withVersion(v)))).await(99.s)
      assert(checked == Left(Problem(
        "The signature's SignerId is unknown: CN=WatchSignatureKeysTest-A")))

    "Sign with changed signature key" in:
      val v = nextVersion()
      val signed = bItemSigner.sign(workflow.withVersion(v))
      controller.api.updateRepo(v, Seq(signed)).await(99.s).orThrow
      testOrder(v)
  }

  "Change signature PEM file slowly" in:
    val signerId = SignerId("CN=WatchSignatureKeysTest-C")
    lazy val cCertAndKey = openssl
      .generateCertWithPrivateKey("TEST", s"/$signerId")
      .orThrow
    lazy val cItemSigner =
      val cSigner = X509Signer.checked(cCertAndKey.privateKey, SHA512withRSA, signerId).orThrow
      new ItemSigner(cSigner, ControllerState.signableItemJsonCodec)

    val controllerUpdated = controller.testEventBus.when[RunningController.ItemSignatureKeysUpdated]
    val agentUpdated = agent.testEventBus.when[Subagent.ItemSignatureKeysUpdated]
    val subagentUpdated = idToAllocatedSubagent(bareSubagentId).allocatedThing.testEventBus
      .when[Subagent.ItemSignatureKeysUpdated]

    X509Cert.fromPem(cCertAndKey.certificatePem).orThrow // Check
    val pem = ByteArray(cCertAndKey.certificatePem).toArray

    autoClosing(new FileOutputStream((controllersKeyDirectory / "key-1.pem").toFile)) { controllerFile =>
      autoClosing(new FileOutputStream((agentsKeyDirectory / "key-1.pem").toFile)) { agentFile =>
        autoClosing(new FileOutputStream((subagentsKeyDirectory / "key-1.pem").toFile)) { subagentFile =>
          val n = 40
          for i <- 0 until n do withClue(s"${(i.s / 10).pretty} -> "):
            for file <- View(controllerFile, agentFile, subagentFile) do
              logger.debug(f"$file write ${pem(i)}%02x")
              file.write(pem(i))
              file.flush()
            sleep(100.ms)
          assert(!controllerUpdated.isCompleted)
          assert(!agentUpdated.isCompleted)
          assert(!subagentUpdated.isCompleted)
          controllerFile.write(pem, n, pem.length - n)
          agentFile.write(pem, n, pem.length - n)
          subagentFile.write(pem, n, pem.length - n)
        }
      }
    }

    Future.sequence(Seq[Future[?]](controllerUpdated, agentUpdated, subagentUpdated)).await(99.s)

    val v = nextVersion()
    controller.api.updateRepo(v, Seq(cItemSigner.sign(workflow.withVersion(v)))).await(99.s).orThrow

    testOrder(v)

  "Ignore invalid certificate (JS-2116)" in:
    val controllerUpdated = controller.testEventBus
      .when[RunningController.ItemSignatureKeysUpdated].map(_ => ())
    val agentUpdated = agent.testEventBus
      .when[Subagent.ItemSignatureKeysUpdated].map(_ => ())
    val subagentUpdated = idToAllocatedSubagent(bareSubagentId).allocatedThing.testEventBus
      .when[Subagent.ItemSignatureKeysUpdated]
      .map(_ => ())

    autoClosing(new FileOutputStream((controllersKeyDirectory / "invalid.pem").toFile)) { controllerFile =>
      autoClosing(new FileOutputStream((agentsKeyDirectory / "invalid.pem").toFile)) { agentFile =>
        autoClosing(new FileOutputStream((subagentsKeyDirectory / "invalid.pem").toFile)) { subagentFile =>
          val invalidPem = "INVALID\n".getBytes(UTF_8)
          controllerFile.write(invalidPem)
          agentFile.write(invalidPem)
          subagentFile.write(invalidPem)
        }
      }
    }

    Future.sequence(Seq[Future[Unit]](controllerUpdated, agentUpdated, subagentUpdated)).await(99.s)

    val v = nextVersion()
    val checked = controller.api.updateRepo(v, Seq(itemSigner.sign(workflow.withVersion(v)))).await(99.s)
    assert(checked == Left(Problem("The signature's SignerId is unknown: CN=WatchSignatureKeysTest-A")))

  private def whenControllerAndAgentUpdated(): Future[Unit] =
    IO.parSequenceN(3)(Seq(
      IO.fromFuture(IO.pure:
          controller.testEventBus.when[RunningController.ItemSignatureKeysUpdated])
        .logWhenItTakesLonger("RunningController.ItemSignatureKeysUpdated"),
      IO.fromFuture(IO.pure:
          agent.testEventBus.when[Subagent.ItemSignatureKeysUpdated])
        .logWhenItTakesLonger("AgentActor.ItemSignatureKeysUpdated"),
      IO.fromFuture(IO.pure:
          idToAllocatedSubagent(bareSubagentId).allocatedThing.testEventBus
            .when[Subagent.ItemSignatureKeysUpdated])
        .logWhenItTakesLonger("BareSubagent.ItemSignatureKeysUpdated"))
    ).void.unsafeToFuture()

  private def testOrder(versionId: VersionId): Unit =
    val events = controller.runOrder(FreshOrder(OrderId(versionId.toString), workflow.path))
      .map(_.value)
    assert(events.last.isInstanceOf[OrderFinished])
    assert(events.head.asInstanceOf[OrderAdded].workflowId == workflow.path ~ versionId)
    assert:
      events.collectFirst:
        case OrderProcessingStarted(Some(`bareSubagentId`), _, _) =>
      .isDefined


object WatchSignatureKeysTest:

  private val logger = Logger[this.type]
  private val agentPath = AgentPath("AGENT")
  private val bareSubagentId = SubagentId("BARE-SUBAGENT")
  private val workflow = Workflow(
    WorkflowPath("WORKFLOW"),
    Seq(
      EmptyJob.execute(agentPath)))

  private val aSignerId = SignerId("CN=WatchSignatureKeysTest-A")
