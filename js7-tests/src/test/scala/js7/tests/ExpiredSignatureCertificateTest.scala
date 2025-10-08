package js7.tests

import cats.effect.IO
import cats.effect.unsafe.{IORuntime, Scheduler}
import java.nio.file.Files.createDirectory
import java.nio.file.{Files, Path}
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.crypt.SignerId
import js7.base.crypt.x509.X509Algorithm.SHA512withRSA
import js7.base.crypt.x509.{Openssl, X509Signer}
import js7.base.io.file.FileUtils.syntax.*
import js7.base.io.file.FileUtils.withTemporaryDirectory
import js7.base.problem.Checked.*
import js7.base.problem.{Checked, Problem}
import js7.base.test.OurTestSuite
import js7.base.thread.CatsBlocking.syntax.await
import js7.base.time.ScalaTime.*
import js7.base.time.TimestampForTests.ts
import js7.base.time.{AlarmClock, TestAlarmClock, Timestamp}
import js7.base.utils.AutoClosing.autoClosing
import js7.base.utils.CatsBlocking
import js7.base.utils.CatsBlocking.BlockingIOResource
import js7.common.utils.FreeTcpPortFinder.findFreeLocalUri
import js7.controller.RunningController
import js7.data.agent.AgentPath
import js7.data.controller.ControllerState
import js7.data.item.{ItemSigner, VersionId}
import js7.data.job.ShellScriptExecutable
import js7.data.order.OrderEvent.{OrderFinished, OrderProcessed}
import js7.data.order.{FreshOrder, OrderEvent, OrderId, OrderOutcome}
import js7.data.subagent.{SubagentId, SubagentItem}
import js7.data.workflow.instructions.Execute
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.subagent.Subagent
import js7.tests.ExpiredSignatureCertificateTest.*
import js7.tests.testenv.DirectoryProvider
import js7.tests.testenv.DirectoryProvider.toLocalSubagentId
import scala.concurrent.TimeoutException
import scala.concurrent.duration.FiniteDuration

final class ExpiredSignatureCertificateTest extends OurTestSuite:

  private given IORuntime = ioRuntime
  private given Scheduler = ioRuntime.scheduler

  private val bareSubagentId = SubagentId("BARE")
  private lazy val bareSubagentItem =
    SubagentItem(bareSubagentId, agentPath, findFreeLocalUri())

  private val nextVersionId = Iterator.from(1).map(i => VersionId(i.toString))

  "All clocks are between notBefore and notAfter" in:
    val now = ts"2050-10-01T12:00:00Z"
    val Right(events) = runMyTest(
      controllerTS = now, directorTS = now, bareSubagentTS = now,
      notBefore = now - 1.s, notAfter = now + 1.s): @unchecked
    assert(events.contains(OrderFinished()))

  "Clock of Controller is after notAfter" in:
    val now = ts"2050-10-01T12:00:00Z"
    val checked = runMyTest(
      controllerTS = now + 2.s, directorTS = now, bareSubagentTS = now,
      notBefore = now - 1.s, notAfter = now + 1.s)
    assert(checked == Left(Problem("The signature's SignerId is unknown: CN=SIGNER")))

  "Clock of Director is after notAfter" in:
    val now = ts"2050-10-01T12:00:00Z"
    // TODO Timeout, because Controller doesn't check failed AgentCommand.AttachSignedItem
    //  nor AgentCommand.AttachOrder, and the order freezes in Attaching.
    intercept[TimeoutException]:
      runMyTest(
        controllerTS = now, directorTS = now + 2.s, bareSubagentTS = now,
        notBefore = now - 1.s, notAfter = now + 1.s,
        timeout = 5.s)

  "Clock of bare Subagent is after notAfter" in:
    val now = ts"2050-10-01T12:00:00Z"
    val Right(events) = runMyTest(
      controllerTS = now, directorTS = now, bareSubagentTS = now + 2.s,
      notBefore = now - 1.s, notAfter = now + 1.s): @unchecked
    assert(events.contains(OrderProcessed(OrderOutcome.Disrupted:
      Problem("The signature's SignerId is unknown: CN=SIGNER"))))

  private def runMyTest(
    controllerTS: Timestamp,
    directorTS: Timestamp,
    bareSubagentTS: Timestamp,
    notBefore: Timestamp,
    notAfter: Timestamp,
    timeout: FiniteDuration = 99.s
  ): Checked[Seq[OrderEvent]] =
    val now = ts"2050-10-01T12:00:00Z"
    val clock = TestAlarmClock(now)
    withTemporaryDirectory("ExpiredSignatureCertificateTest-"): workDir =>
      val controllerClock = TestAlarmClock(controllerTS)
      val directorClock = TestAlarmClock(directorTS)
      withDirectoryProvider(controllerClock, directorClock): directoryProvider =>
        directoryProvider.bareSubagentEnvResource(bareSubagentItem,
          director = directorsSubagentId,
          extraConfig = config"""
            js7.configuration.trusted-signature-keys.X509 =
              $${js7.config-directory}"/private/trusted-x509-keys"
            """
        ).blockingUse(timeout): bareSubagentEnv =>
          val aCertAndKey = Openssl(workDir).generateCertWithPrivateKey("TEST", s"/$aSignerId",
              notBefore = Some(notBefore),
              notAfter = Some(notAfter))
            .orThrow
          val itemSigner = ItemSigner(
            X509Signer.checked(aCertAndKey.privateKey, SHA512withRSA, aSignerId).orThrow,
            ControllerState.signableItemJsonCodec)

          distributeCertificates(directoryProvider, aCertAndKey)

          val subagentTestWiring = Subagent.TestWiring(clock = TestAlarmClock(bareSubagentTS))
          bareSubagentEnv.subagentResource(subagentTestWiring).blockingUse(timeout): subagent =>
            directoryProvider.run: (controller, _) =>
              // Force the director to use the bare subagent
              controller.enableSubagents(directorsSubagentId -> false)

              val v = nextVersionId.next()
              val workflow = simpleWorkflow.withVersion(v)
              val signedWorkflow = itemSigner.sign(workflow)

              controller.api.updateRepo(v, Seq(signedWorkflow)).await(timeout).map: _ =>
                controller.runOrder(
                  FreshOrder(OrderId("ORDER"), workflow.path, deleteWhenTerminated = true),
                  timeout = timeout)
                .map(_.value)

  private def withDirectoryProvider[A](controllerClock: AlarmClock, directorClock: AlarmClock)
    (body: DirectoryProvider => A)
  : A =
    autoClosing(
      DirectoryProvider(
        testName = Some("ExpiredSignatureCertificateTest"),
        agentPaths = Seq(agentPath),
        bareSubagentItems = Seq(bareSubagentItem),
        controllerConfig = config"""
          js7.auth.users.TEST-USER.permissions = [ UpdateItem ]
          js7.configuration.trusted-signature-keys.X509 =
            $${js7.config-directory}"/private/trusted-x509-keys"
          """,
        agentConfig = config"""
          js7.job.execution.signed-script-injection-allowed = yes
          js7.configuration.trusted-signature-keys.X509 =
            $${js7.config-directory}"/private/trusted-x509-keys"
          """,
        controllerTestWiring = RunningController.TestWiring(alarmClock = Some(controllerClock)),
        subagentTestWiring = Subagent.TestWiring(clock = directorClock))
    ):
      body

  private def distributeCertificates(
    directoryProvider: DirectoryProvider,
    certWithKey: Openssl.CertWithPrivateKey)
  : Unit =
    val controllerKeyDir = directoryProvider.controllerEnv.trustedX509KeyDir
    val agentKeyDir = directoryProvider.agentEnvs.head.trustedX509KeyDir
    val bareSubagentKeyDir = directoryProvider
      .bareSubagentToDirectory(bareSubagentId) / "config" / "private" / "trusted-x509-keys"

    writeCertificate(controllerKeyDir, certWithKey.certificatePem)
    writeCertificate(agentKeyDir, certWithKey.certificatePem)
    writeCertificate(bareSubagentKeyDir, certWithKey.certificatePem)

  private def writeCertificate(dir: Path, pem: String): Unit =
    if !Files.exists(dir) then createDirectory(dir)
    dir / "test.pem" := pem


object ExpiredSignatureCertificateTest:

  private val agentPath = AgentPath("AGENT")
  private val directorsSubagentId = toLocalSubagentId(agentPath)
  private val aSignerId = SignerId("CN=SIGNER")

  private val simpleWorkflow = Workflow(WorkflowPath("WORKFLOW"), Seq(
    Execute(WorkflowJob(agentPath, ShellScriptExecutable(":")))))
